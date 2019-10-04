module FormConfig = {
  type field(_) =
    | Name: field(string)
    | Email: field(string)
    | Picture: field(string)
    | Description: field(string);

  type state = {
    name: string,
    email: string,
    picture: string,
    description: string,
  };

  let get: type value. (state, field(value)) => value =
    (state, field) =>
      switch (field) {
      | Name => state.name
      | Email => state.email
      | Picture => state.picture
      | Description => state.description
      };

  let set: type value. (state, field(value), value) => state =
    (state, field, value) =>
      switch (field) {
      | Name => {...state, name: value}
      | Email => {...state, email: value}
      | Picture => {...state, picture: value}
      | Description => {...state, description: value}
      };
};

module Form = ReFormNext.Make(FormConfig);

let testName = (str: string) => {
  let exp = () => [%re "/^[a-z0-9._]{0,30}$/i"];
  let res = exp() |> Js.Re.exec(str);
  switch (res) {
  | Some(_) => true
  | _ => false
  };
};

let withUserForm =
    (id, name, email, picture, description, mutation, innerComponentFn) =>
  Form.make(
    ~initialState={name, email, picture, description},
    ~onSubmit=
      values =>
        UserUpdate.mutate(
          mutation,
          values.state.values.name,
          values.state.values.email,
          values.state.values.picture,
          values.state.values.description,
          id,
        ),
    ~schema=
      Form.Validation.Schema([|
        Custom(
          Name,
          values => testName(values.name) ? Valid : Error(Lang.wrongName),
        ),
      |]),
    innerComponentFn,
  )
  |> E.React.el;

let formFields = (form: Form.state, send, onSubmit, getFieldState) => {
  let stateName: Form.fieldState = getFieldState(Form.Field(Name));

  let error = state =>
    switch (state) {
    | Form.Error(s) => <AntdAlert message=s type_="warning" />
    | _ => ReasonReact.null
    };

  let isFormValid =
    switch (stateName) {
    | Form.Valid => true
    | Form.Pristine => false
    | _ => false
    };

  <Antd.Form onSubmit={_e => onSubmit()}>
    <Antd.Form.Item>
      {"Username" |> Utils.ste |> E.React.inH3}
      <Antd.Input
        value={form.values.name}
        onChange={ReForm.Helpers.handleDomFormChange(e => {
          send(Form.FieldChangeValue(Name, e));

          ();
        })}
      />
      {error(stateName)}
    </Antd.Form.Item>
    <Antd.Form.Item>
      {"Description" |> Utils.ste |> E.React.inH3}
      <Antd.Input
        value={form.values.description}
        onChange={ReForm.Helpers.handleDomFormChange(e =>
          send(Form.FieldChangeValue(Description, e))
        )}
      />
    </Antd.Form.Item>
    <Antd.Form.Item>
      {"Email" |> Utils.ste |> E.React.inH3}
      <AntdInput
        value={form.values.email}
        disabled=true
        onChange={ReForm.Helpers.handleDomFormChange(e =>
          send(Form.FieldChangeValue(Email, e))
        )}
      />
    </Antd.Form.Item>
    <Antd.Form.Item>
      {"Picture URL" |> Utils.ste |> E.React.inH3}
      <Antd.Input
        value={form.values.picture}
        onChange={ReForm.Helpers.handleDomFormChange(e =>
          send(Form.FieldChangeValue(Picture, e))
        )}
      />
    </Antd.Form.Item>
    <Antd.Form.Item>
      <Antd.Button
        _type=`primary onClick={_ => onSubmit()} disabled={!isFormValid}>
        {"Submit" |> Utils.ste}
      </Antd.Button>
    </Antd.Form.Item>
  </Antd.Form>;
};

module CMutationForm =
  MutationForm.Make({
    type queryType = UserUpdate.EditUser.t;
  });

let component = ReasonReact.statelessComponent("Profile");
let make =
    (
      ~loggedInUser: Types.user,
      ~layout=SLayout.FullPage.makeWithEl,
      _children,
    ) => {
  ...component,
  render: _ =>
    SLayout.LayoutConfig.make(
      ~head=SLayout.Header.textDiv("Edit Profile Information"),
      ~body=
        <FC.PageCard.BodyPadding>
          {UserUpdate.withUserMutation((mutation, data) => {
             let agent = loggedInUser.agent;
             let id = loggedInUser.id;
             let email = loggedInUser.email |> E.O.default("");
             let picture = loggedInUser.picture |> E.O.default("");
             let description = loggedInUser.description |> E.O.default("");
             let name =
               agent
               |> E.O.bind(_, (r: Types.agent) => r.name)
               |> E.O.toExn("The logged in user needs an ID!");

             withUserForm(
               id,
               name,
               email,
               picture,
               description,
               mutation,
               ({send, state, getFieldState}) =>
               CMutationForm.showWithLoading(
                 ~result=data.result,
                 ~form=
                   formFields(
                     state,
                     send,
                     () => send(Form.Submit),
                     getFieldState,
                   ),
                 (),
               )
             );
           })}
        </FC.PageCard.BodyPadding>,
    )
    |> layout,
};