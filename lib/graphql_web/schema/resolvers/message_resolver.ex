defmodule GraphqlWeb.Schema.Resolvers.MessageResolver do
  alias Graphql.Chat
  alias Graphql.Message
  # alias GraphqlWeb.Utils.Utils
  alias GraphqlWeb.Constants.Constants

  def get_all_messages(_, _, %{context: _context}) do
    IO.puts("get all messages func")
    messages = Message.list_messages()
    {:ok, messages}
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
