defmodule HighSchoolSweetheart do
  def first_letter(name), do: name |> String.trim |> String.at(0)
  def initial(name), do: "#{first_letter(name)}." |> String.upcase

  def initials(full_name) do
    full_name |> String.split |> Enum.map(&initial/1) |> Enum.join(" ")
  end

  def pair(full_name1, full_name2) do
    """
         ******       ******
       **      **   **      **
     **         ** **         **
    **            *            **
    **                         **
    **     A. B.  +  C. D.     **
     **                       **
       **                   **
         **               **
           **           **
             **       **
               **   **
                 ***
                  *
    """ |> String.replace("A. B.", initials(full_name1)) |> String.replace("C. D.", initials(full_name2))
  end
end
