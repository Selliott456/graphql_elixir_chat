defmodule Graphql.Chat.Room do
  alias Graphql.Auth.User
  use Ecto.Schema
  import Ecto.Changeset

  schema "rooms" do
    field :name, :string
    field :description, :string


    belongs_to :users, User

    timestamps()
  end

  @doc false
  def changeset(room, attrs) do
    room
    |> cast(attrs, [:name, :description, :user_id])
    |> validate_required([:name, :description])
    |> unique_constraint(:name)
    |> validate_length(:name, min: 5, max: 30)
    |> validate_length(:description, min: 10, max: 300)
  end
end
