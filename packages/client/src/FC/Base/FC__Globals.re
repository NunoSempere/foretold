let load = () => {
  Css.(
    global(
      "body",
      [
        fontFamily(FC__Colors.Text.standardFont),
        margin(`zero),
        height(`percent(100.0)),
        background(`hex("F0F1F3")),
        fontSize(`px(16)),
      ],
    )
  );

  Css.(global("html", [height(`percent(100.0))]));
};