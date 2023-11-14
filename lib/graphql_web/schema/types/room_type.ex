defmodule GraphqlWeb.Schema.Types.RoomType do
  use Absinthe.Schema.Notation
  import_types(GraphqlWeb.Schema.Types.UserType)


  object :room_type do

    field :id, :id
    field :user_id, :id
    field :user, :user_type
    field :name, :string
    field :description, :string
    field :inserted_at, :string

  end
end
