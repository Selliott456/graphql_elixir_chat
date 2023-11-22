defmodule GraphqlWeb.Schema.Types.RoomType do
  use Absinthe.Schema.Notation

  object :room_type do

    field :id, :id
    field :user_id, :id
    field :user, :user_type
    field :name, :string
    field :description, :string
    field :inserted_at, :string
  end

  input_object :room_input_type do
    field :name, non_null(:string)
    field :description, non_null(:string)
  end
end
