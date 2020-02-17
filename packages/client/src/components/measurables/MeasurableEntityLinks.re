let kenDisplay = (g, id) => {
  KenTools.Subject.facts(g, id)
  |> E.A.of_list
  |> E.A.fmapi((i, r: KenTools.Fact.fact) =>
       <div key={i |> string_of_int}>
         {KenTools.Subject.name(g, r.propertyId)
          |> E.O.bind(_, KenTools.FactValue.toString)
          |> E.O.default("no-name")
          |> Utils.ste
          |> E.React2.inH3}
         KenTools.(
           switch (r.value.valueType) {
           | String(s) => s |> Utils.ste
           | ThingId(s) =>
             <Link linkType={Internal(EntityShow(s))}>
               {s |> Utils.ste}
             </Link>
           | _ => "no-name" |> Utils.ste
           }
         )
       </div>
     )
  |> ReasonReact.array;
};

let xEntityLink = (attribute, ~g, ~m: Types.measurable, ~className: string) =>
  m
  |> attribute
  |> E.O.bind(_, KenTools.Subject.name(g))
  |> E.O.bind(_, KenTools.FactValue.toString)
  |> E.O.bind(_, r =>
       m
       |> attribute
       |> E.O.fmap(_d =>
            <Antd_Popover
              content={kenDisplay(g, attribute(m) |> E.O.default(""))}
              trigger=`hover
              placement=`top>
              <span className> {r |> Utils.ste} </span>
            </Antd_Popover>
          )
     );

let nameEntityLink = xEntityLink(r => r.labelSubject);
let propertyEntityLink = xEntityLink(r => r.labelProperty);