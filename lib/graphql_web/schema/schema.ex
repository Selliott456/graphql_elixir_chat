defmodule GraphqlWeb.Schema do
  use Absinthe.Schema
  alias GraphqlWeb.Schema.Resolvers

  import_types(GraphqlWeb.Schema.Types.Types)

  @desc "greet"
  query do
    field :hello, :string do
      resolve(fn _, _, _ -> {:ok, "worlds"} end)
    end
  @dec "get all users"
    field :users, list_of(:user_type) do
      resolve(&Resolvers.UserResolver.get_all_users/3)
    end
  end

  mutation do
    field :register_user, :boolean do
      arg(:input, non_null(:registration_input_type))
      resolve(&Resolvers.UserResolver.register_user/3)
    end
  end
end
