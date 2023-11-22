defmodule Graphql.Chat.Message do
  alias Graphql.Chat.Room
  alias Graphql.Auth.User
  use Ecto.Schema
  import Ecto.Changeset

  schema "messages" do
    field :content, :string

    belongs_to :user, User
    belongs_to :room, Room

    timestamps()
  end

  @doc false
  def changeset(message, attrs) do
    message
    |> cast(attrs, [:content, :user_id, :room_id])
    |> validate_required([:content, :user_id, :room_id])
    |> validate_length(:content, min: 1)
  end
end
