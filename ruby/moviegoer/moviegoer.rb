# coding: utf-8
class NotMovieClubMemberError < RuntimeError
end

class Moviegoer
  def initialize(age, member: false)
    @age = age
    @member = member
  end

  def ticket_price
    @age < 60 ? 15.0 : 10.0
  end

  def watch_scary_movie?
    @age >= 18
  end

  def claim_free_popcorn!
    if @member then "🍿"
    else
      raise NotMovieClubMemberError.new("pop corn crasher, eh?")
    end
  end
end
