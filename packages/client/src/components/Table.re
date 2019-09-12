type column('a) = {
  name: ReasonReact.reactElement,
  render: 'a => ReasonReact.reactElement,
  flex: int,
  show: 'a => bool,
};

let filterColums = (columns, rows) => {
  columns
  |> Js.Array.filter(column =>
       rows |> Js.Array.find(column.show) |> E.O.toBool
     );
};

module Column = {
  let make = (~name, ~render, ~flex=1, ~show=_ => true, ()): column('b) => {
    name,
    render,
    flex,
    show,
  };
};

let fromColumns =
    (
      columns: array(column('a)),
      rows: array('a),
      ~bottomSubRowFn: option('a => option(array(ReasonReact.reactElement)))=None,
      ~onRowClb: 'a => unit=_ => (),
      (),
    ) => {
  let columns' = filterColums(columns, rows);

  <Table>
    <FC__Table.HeaderRow>
      {columns'
       |> Array.mapi((columnIndex, column: column('a)) =>
            <FC.Table.Cell
              flex={`num(column.flex |> float_of_int)}
              key={columnIndex |> string_of_int}>
              {column.name}
            </FC.Table.Cell>
          )
       |> ReasonReact.array}
    </FC__Table.HeaderRow>
    {rows
     |> Array.mapi((rowIndex, row: 'a) => {
          let columnsBody =
            columns'
            |> Array.mapi((columnIndex, column: column('a)) =>
                 <FC.Table.Cell
                   flex={`num(column.flex |> float_of_int)}
                   key={columnIndex |> string_of_int}>
                   {column.render(row)}
                 </FC.Table.Cell>
               )
            |> ReasonReact.array;

          let key = rowIndex |> string_of_int;
          let bottomSubRow = bottomSubRowFn |> E.O.bind(_, r => r(row));

          <FC.Table.Row
            onClick={_ => {
              onRowClb(row);
              ();
            }}
            ?bottomSubRow
            key>
            columnsBody
          </FC.Table.Row>;
        })
     |> ReasonReact.array}
  </Table>;
};