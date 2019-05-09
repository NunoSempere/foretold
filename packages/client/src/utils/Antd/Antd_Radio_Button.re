[@bs.module "antd/lib/radio/radioButton"]
external reactClass: ReasonReact.reactClass = "default";

[@bs.obj]
external makeProps:
  (~value: string, ~onClick: ReactEvent.Form.t => unit=?, unit) => _ =
  "";

let make = (~value, ~onClick, children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass,
    ~props=makeProps(~value, ~onClick, ()),
    children,
  );