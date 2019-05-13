module type Config = {type queryType;};
open Utils;

module Make = (Config: Config) => {
  let showWithLoading =
      (
        ~result: ReasonApolloTypes.mutationResponse(Config.queryType),
        ~form,
        ~successMessage="Update Successful",
        (),
      ) =>
    switch (result) {
    | Loading => "Loading" |> ste
    | Error(e) => <> {"Error: " ++ e##message |> ste} form </>
    | Data(_) => successMessage |> ste |> E.React.inH2
    | NotCalled => form
    };
};