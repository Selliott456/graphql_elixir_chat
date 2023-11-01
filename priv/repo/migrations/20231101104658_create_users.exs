defmodule Graphql.Repo.Migrations.CreateUsers do
  use Ecto.Migration
  import Ecto.Changeset

  def change do
    create table(:users) do
      add :name, :string
      add :email, :string
      add :username, :string
      add :password, :string

      timestamps()
    end

    create unique_index(:users, [:username])
    create unique_index(:users, [:email])
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email, :username, :password])
    |> validate_required([:name, :email, :username, :password])
    |> validate_length(:password, min: 8, max: 30)
    |> validate_length(:username, min: 3, max: 30)
    |> validate_length(:name, min: 2, max: 30)

  end
end
