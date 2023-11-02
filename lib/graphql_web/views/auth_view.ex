defmodule GraphqlWeb.AuthView do
use GraphqlWeb, :view

def render("acknowledge.json", %{message: message}) do
  %{success: true, message: message}
end
end
