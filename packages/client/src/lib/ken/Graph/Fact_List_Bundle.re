open Graph_T.T;
open Graph_T;

type factList = Graph_Fact_Filters.t;

type t = {
  graph: Graph_T.T.t,
  list: factList,
  thing,
};

let init = (g: Graph_T.T.t, t: Graph_T.T.thing): t => {
  graph: g,
  list: g |> Graph_T.F.factList,
  thing: t,
};

module Internal = {
  let findFromList = (id, t) =>
    t |> E.L.find(e => e.thingId.thingIdString == id);

  let unpackOptionList = (e: list(option('a))) =>
    e |> E.L.filter(E.O.isSome) |> E.L.fmap(E.O.toExn("mistake"));

  let filterFacts = (filter: (string, factList) => factList, t: t) => {
    ...t,
    list: filter(Thing.id(t.thing), t.list),
  };

  let facts = filterFacts(Graph_Fact_Filters.withIdAsAnyEdge);

  let isEdgeForFacts = edge =>
    filterFacts(Graph_Fact_Filters.withEdge(edge));

  let filterFactsAndSelectThings = (fromEdge, toEdge, t: t) =>
    t
    |> filterFacts(Graph_Fact_Filters.withEdge(fromEdge))
    |> (e => e.list)
    |> E.L.fmap(Graph_Graph.findThingFromFact(t.graph, toEdge))
    |> unpackOptionList;

  let connectedPropertyThings = filterFactsAndSelectThings(SUBJECT, PROPERTY);

  let connectedSubjectThings = filterFactsAndSelectThings(PROPERTY, SUBJECT);

  let connectedPropertyWithId = (id: string, t: t) =>
    connectedPropertyThings(t) |> findFromList(id);

  let connectedSubjectWithId = (id: string, t: t) =>
    connectedSubjectThings(t) |> findFromList(id);
};