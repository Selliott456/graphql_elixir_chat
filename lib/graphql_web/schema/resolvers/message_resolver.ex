defmodule GraphqlWeb.Schema.Resolvers.MessageResolver do
  alias Graphql.Message
  alias GraphqlWeb.Utils.Utils
  alias GraphqlWeb.Constants.Constants

  def get_all_messages(_, %{input: input}, %{context: _context}) do
    messages = Message.list_messages(input.room_id)
    {:ok, messages}
  end

  def create_message(_, %{input: input}, %{context: context}) do
    input_with_ids = Map.merge(input, %{user_id: context.current_user.id, room_id: input.room_id})

    case Message.create_message(input_with_ids) do
      {:ok, _message} ->
        {:ok, true}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:error, Utils.format_changeset_errors(changeset)}

      _ ->
        {:error, Constants.internal_server_error}
    end

  end

  # def delete_message(_, %{input: input}, %{context: context}) do
  #   case Chat.delete_room_by_id(input.room_id, context.current_user.id) do
  #     {1, _} ->
  #       {:ok, true}

  #     {0, _} ->
  #       {:error, Constants.not_found}

  #       _ ->
  #         {:error, Constants.internal_server_error}
  #   end
  # end
end
