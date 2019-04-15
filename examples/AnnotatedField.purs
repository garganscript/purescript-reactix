annotatedField :: Props -> Element
annotatedField p = createElement annotatedFieldComponent p []

annotatedFieldComponent :: Memo Props
annotatedFieldComponent props = memo component hasChanged where
  hasChanged o n = o.ngrams /= n.ngrams || o.text /= n.text
  component props = do
    state <- useState $ pure (compile props)
    let children = run <$> state.value.runs
    p [className "annotated-field", contextMenuHandler] children
  run (Tuple txt lst)
    | Just list <- lst = span [termStyle list] [text txt]
    | otherwise = span [] [text txt]

-- vs

annotatedFieldClass :: ReactClass (WithChildren Props')
annotatedFieldClass =
  React.component "AnnotatedField"
    (\this -> do
       -- log $ "AnnotatedField: constructing"
       s <- spec this
       pure { state : s.state
            , render: s.render
            , componentDidUpdate: \old _s _snap -> do
                new <- React.getProps this
                when (old.ngrams /= new.ngrams || old.text /= new.text) do
                  -- log "AnnotatedField: forcing refresh"
                  dispatcher this ForceRefresh
            })
  where
    performAction :: PerformAction State Props Action
    performAction ForceRefresh = forceRefresh
    performAction _ = \_ _ -> pure unit
    -- performAction (ShowContextMenu i) = showContextMenu i
    -- performAction (AddTerm t l) = addTerm t l
    -- performAction = defaultPerformAction
    render :: Render State Props Action
    render d _p s _c = [ p [className "annotated-field", onContextMenu $ contextMenuHandler (d ShowContextMenu)] $ children s.runs ]
    children = fromFoldable <<< map renderRun
    renderRun (Tuple txt lst)
      | Just list <- lst = span [termStyle list] [text txt]
      | otherwise = span [] [text txt]
    {spec, dispatcher} = createReactSpec (simpleSpec performAction render) compile

