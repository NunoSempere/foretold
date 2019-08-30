open FC;
open Base;

let make =
  <PageCard>
    <PageCard.HeaderRow>
      <Div float=`left>
        <PageCard.HeaderRow.Title>
          {"Pending Resolution" |> ReasonReact.string}
        </PageCard.HeaderRow.Title>
      </Div>
      <Div
        float=`right
        className={Css.style([
          PageCard.HeaderRow.Styles.itemTopPadding,
          PageCard.HeaderRow.Styles.itemBottomPadding,
        ])}>
        <FC__Button variant=Primary>
          {"< Back" |> ReasonReact.string}
        </FC__Button>
      </Div>
    </PageCard.HeaderRow>
    <Table.HeaderRow>
      <Table.Cell flex={`num(4.)}>
        {"Name & Status" |> ReasonReact.string}
      </Table.Cell>
      <Table.Cell flex={`num(2.)}>
        {"Aggregate Prediction" |> ReasonReact.string}
      </Table.Cell>
      <Table.Cell flex={`num(1.)}>
        {"Details" |> ReasonReact.string}
      </Table.Cell>
    </Table.HeaderRow>
  </PageCard>;