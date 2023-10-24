package scalix
import scala.io.Source
import org.json4s.*

implicit val formats: DefaultFormats.type = DefaultFormats

import org.json4s.native.JsonMethods._
val api_key = "api_key=097342ccbc3fe2613b6a792085e110d7"

object Scalix extends App{
  val url = "https://api.themoviedb.org/3/search/person?query=tom-cruise&"+api_key
  val source = Source.fromURL(url)
  val contents = source.mkString
  println("Search Tom Cruise: \n"+contents)

  val json = parse(contents)
  println("Format type Json : \n"+json)

  println(s"\n -------------STARTING TEST ------------\n")

  /*-- Test findActorId*/
  val actor_name = "Tom Cruise"
  val actorFromId = findActorId("Tom", "Cruise")
  println(s"Find $actor_name's id :\n $actorFromId\n")

  if (actorFromId.isDefined){
    /*-- Test findActorMovies*/
    val actorMoviesFromActorId = findActorMovies(actorFromId.get)
    println(s"Find actor id=$actorFromId movies :\n $actorMoviesFromActorId\n")

    /*-- Test findMovieDirector*/
    val movieId= actorMoviesFromActorId.head._1
    val movieDirector = findMovieDirector(movieId)
    println(s"Find movie id=$movieId director's id and name :\n $movieDirector\n")

    /*-- Test findMovieDirector*/
    val actor1= FullName("Éric", "Judor")
    val actor2= FullName("Ramzy", "Bedia")
    val collab: Set[(String, String)] = collaboration(actor1, actor2)
    println(s"Collaboration entre $actor1 et $actor2 Taille ${collab.size}:\n $collab")
  }

  //retourne l'entier identifiant un acteur à partir de son nom et de son prénom
  def findActorId(name: String, surname: String): Option[Int] = {
    val url = s"https://api.themoviedb.org/3/search/person?query=$name+$surname+&$api_key"
    val source = Source.fromURL(url)
    val json = parse(source.mkString)

    val request_results = (json \ "results" \ "id").children.head.extractOpt[Int]
    request_results
  }

  //retourne l'ensemble des films dans lequel a tourné cet acteur sous la forme de paires donnant l'identifiant du film et son titre
  def findActorMovies(actorId: Int): Set[(Int, String)] = {
    val url = s"https://api.themoviedb.org/3/person/$actorId/movie_credits?$api_key"
    val source = Source.fromURL(url)
    val contents = source.mkString
    val json = parse(contents)

    val id_list = (json \ "cast" \ "id").extract[List[Int]]
    val title_list = (json \ "cast" \ "title").extract[List[String]]

    var res: Set[(Int, String)] = Set()
    for (i <- id_list.indices) {
      res += (id_list(i), title_list(i))
    }
    res
  }

  //retourne le réalisateur d'un film sous la forme d'une paire donnant son identifiant et son nom
  def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    val url = s"https://api.themoviedb.org/3/movie/$movieId/credits?$api_key"
    val source = Source.fromURL(url)
    val contents = source.mkString
    val json = parse(contents)

    //List[JValue], Director in "crew"
    val crew = (json \ "crew").children

    var director : Option[(Int,String)] = None
    for (elem <- crew) {
      if ((elem \ "job").extractOpt[String].contains("Director")) {
        val id = (elem \ "id").extract[Int]
        val name = (elem \ "name").extract[String]
        director = Some((id, name))
        return director
      }
    }
    director
  }

  case class FullName(first:String, last:String)

  def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] = {
    val id_actor_1 = findActorId(actor1.first, actor1.last)
    val id_actor_2 = findActorId(actor2.first, actor2.last)

    if (id_actor_1.isDefined && id_actor_2.isDefined) {
      //Set([Int,String])
      val movies_1 = findActorMovies(id_actor_1.get)
      val movies_2 = findActorMovies(id_actor_2.get)
      //println(s"Movies 1 de $actor1 Taille ${movies_1.size}: \n$movies_1\n")
      //println(s"Movies 2 de $actor2 Taille ${movies_2.size}: \n$movies_2\n")

      val res = movies_1.intersect(movies_2)

      var director_movie: Set[(String, String)] = Set()
      var tmp : Option[(Int,String)]= None

      for(m <- res){
        tmp=findMovieDirector(m._1)
        if(tmp.isDefined){
          director_movie+= (tmp.get._2, m._2)
        }
      }
      return director_movie
    }
    Set()
  }
}
