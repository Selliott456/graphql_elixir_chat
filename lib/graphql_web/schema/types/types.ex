defmodule GraphqlWeb.Schema.Types.Types do
  use Absinthe.Schema.Notation

  import_types(GraphqlWeb.Schema.Types.UserType)
end
