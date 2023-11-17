defmodule Graphql.Auth do
  @moduledoc """
  The Auth context.
  """

  import Ecto.Query, warn: false
  alias Graphql.Repo

  alias Graphql.Auth.User


  def list_users do
    Repo.all(from(u in User, preload: [:rooms]))
  end


  def get_user!(id), do: Repo.get!(User, id)

  def get_by_username(username) do
    Repo.get_by(User, username: username)
  end

  def create_user(attrs \\ %{}) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end


  def update_user(%User{} = user, attrs) do
    user
    |> User.changeset(attrs)
    |> Repo.update()
  end

  def delete_user(%User{} = user) do
    Repo.delete(user)
  end

  def change_user(%User{} = user, attrs \\ %{}) do
    User.changeset(user, attrs)
  end
end
