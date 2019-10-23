open FC;
open Base;

let pastTime = 1483232400;
let format_standard = "LLL";

let cdf = ExampleCdfs.Example1.cdf;

let cellStyle =
  Css.(style([paddingTop(`em(0.7)), paddingBottom(`em(0.4))]));

let row =
  <Table.Row
    bottomSubRow=[|
      Table.Row.textSection(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut vulputate tortor a sapien aliquet ullamcorper. Nunc non varius sapien, quis elementum sapien. Morbi ac tristique quam. Cras hendrerit accumsan pretium. Praesent id nisl sit amet eros imperdiet placerat. Vestibulum sodales posuere diam vel laoreet.
        "
        |> ReasonReact.string,
      ),
    |]>
    <Table.Cell
      flex={`num(2.0)}
      className=Css.(
        style([paddingTop(`em(0.6)), paddingBottom(`em(0.0))])
      )>
      <FC__CdfChart__Plain.Jsx2
        cdf
        minX=2.0
        color={`hex("#d9dcdf")}
        maxX=12.0
      />
    </Table.Cell>
    <Table.Cell
      flex={`num(1.0)}
      className=Css.(
        style([paddingTop(`em(0.2)), paddingBottom(`em(0.3))])
      )>
      <FC__CdfChart__StatSummary.Jsx2 cdf />
    </Table.Cell>
    <Table.Cell flex={`num(1.0)} className=cellStyle>
      <FC__AgentLink.Jsx2
        agent={FC__AgentLink.Agent.makeUser(
          ~name="Roger Adams",
          ~image=
            "https://lh3.googleusercontent.com/-1sj3EqkojJ4/AAAAAAAAAAI/AAAAAAAAAAA/ACHi3rfWCVqnuJxxM41Zird4HZx0BbRpbQ/photo.jpg",
          (),
        )}
      />
    </Table.Cell>
    <Table.Cell flex={`num(1.0)} className=cellStyle>
      <span className=Css.(style([color(FC__Settings.textMedium)]))>
        {MomentRe.momentWithUnix(pastTime)
         |> MomentRe.Moment.format(format_standard)
         |> ReasonReact.string}
      </span>
    </Table.Cell>
  </Table.Row>;

let row2 =
  <Table.Row>
    <Table.Cell
      flex={`num(2.0)}
      className=Css.(
        style([paddingTop(`em(0.6)), paddingBottom(`em(0.0))])
      )>
      <FC__CdfChart__Plain.Jsx2
        cdf
        minX=2.0
        color={`hex("#d9dcdf")}
        maxX=12.0
      />
    </Table.Cell>
    <Table.Cell
      flex={`num(1.0)}
      className=Css.(
        style([paddingTop(`em(0.2)), paddingBottom(`em(0.3))])
      )>
      <FC__CdfChart__StatSummary.Jsx2 cdf />
    </Table.Cell>
    <Table.Cell flex={`num(1.0)} className=cellStyle>
      <FC__AgentLink.Jsx2
        agent={FC__AgentLink.Agent.makeUser(
          ~name="Samantha Hope",
          ~image=
            "https://lh3.googleusercontent.com/-1sj3EqkojJ4/AAAAAAAAAAI/AAAAAAAAAAA/ACHi3rfWCVqnuJxxM41Zird4HZx0BbRpbQ/photo.jpg",
          (),
        )}
      />
    </Table.Cell>
    <Table.Cell flex={`num(1.0)} className=cellStyle>
      <span className=Css.(style([color(FC__Settings.textMedium)]))>
        {MomentRe.momentWithUnix(pastTime)
         |> MomentRe.Moment.format(format_standard)
         |> ReasonReact.string}
      </span>
    </Table.Cell>
  </Table.Row>;

let make =
  <PageCard>
    <PageCard.HeaderRow>
      <Div.Jsx2>
        <Div.Jsx2
          styles=[
            Css.style([BaseStyles.floatLeft, Css.paddingTop(`em(0.2))]),
          ]>
          <Tab.Jsx2 isActive=true>
            {"Predictions" |> ReasonReact.string}
          </Tab.Jsx2>
          <Tab.Jsx2 isActive=false>
            {"Settings" |> ReasonReact.string}
          </Tab.Jsx2>
        </Div.Jsx2>
      </Div.Jsx2>
      <Div.Jsx2>
        <Div.Jsx2
          float=`right
          styles=[Css.style([PageCard.HeaderRow.Styles.itemTopPadding])]>
          {PaginationButtons.make({
             currentValue: Range(3, 10),
             max: 100,
             pageLeft: {
               isDisabled: false,
               onClick: _ => (),
             },
             pageRight: {
               isDisabled: true,
               onClick: _ => (),
             },
           })}
        </Div.Jsx2>
      </Div.Jsx2>
    </PageCard.HeaderRow>
    <Div.Jsx2 styles=[Css.style(BaseStyles.fullWidthFloatLeft)]>
      <Table>
        <Table.HeaderRow>
          <Table.Cell flex={`num(2.0)}>
            {"Prediction Distribution" |> ReasonReact.string}
          </Table.Cell>
          <Table.Cell flex={`num(1.0)}>
            {"Prediction Value" |> ReasonReact.string}
          </Table.Cell>
          <Table.Cell flex={`num(1.0)}>
            {"Agent" |> ReasonReact.string}
          </Table.Cell>
          <Table.Cell flex={`num(1.0)}>
            {"Time" |> ReasonReact.string}
          </Table.Cell>
        </Table.HeaderRow>
        row
        row2
        row2
        row2
        row
        row
        row2
        row2
      </Table>
    </Div.Jsx2>
  </PageCard>;
