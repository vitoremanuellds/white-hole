:- module(movie_operations, [
        avaluateMovie/6,
        getMovieIdFromRatings/3,
        getRatings/3
    ]).
:- use_module('./util.pl').
:- use_module('../Db/dbOperations.pl').
:- use_module('./user_operations.pl').


avaluateMovie(Connection, User, Movie, Rating, Commentary, Confirmacao):-
    User = row(Email, _, _, _),
    Movie = row(MovieId, _, _, _, _, _),
    user_operations:userAlreadyExists(Connection, Email, Fstconf),
    getAvaluations(Connection, Email, Ratings),
    getMovieIdFromRatings(Ratings, [], MoviesIds),
    (Fstconf =:= 1, member(Rating, [1,2,3,4,5]), not(member(MovieId, MoviesIds)) -> 
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO ratings (useremail, movieid, rating, commentary) values ('%w', %w, %w, '%w')",
            [Email, MovieId, Rating, Commentary]
        ), Confirmacao is 1
        ;
        Confirmacao is 0
    ).


getMovieIdFromRatings([Rating|T], ResultTemp, Result):-
    length(T, L),
    (L > 0 ->
        Rating = row(, , MovieId, , ), 
        getMovieIdFromRatings(T, [ MovieId | ResultTemp ], Result);
        Rating = row(, , MovieId, , ),
        reverse([ MovieId | ResultTemp ], Result)
    ).


getRatings(Connection, Movie, Ratings):-
    Movie = row(MovieId, _, _, _, _, _),
    dbop:db_parameterized_query(
        Connection,
        "SELECT * FROM ratings WHERE movieid = %w;",
        [MovieId],
        Ratings
    ).


getCasting(Connection, Movie, Casting):-
    Movie = row(MovieId, _, _, _, _, _),
    dbop:db_parameterized_query(
        Connection,
        "SELECT name, movieFunction FROM casting WHERE movieid = %w;",
        [MovieId],
        Casting
    ).


titleContainsWordMovie([Word|T], Movie, Confirmacao):-
    Movie = row(MovieId, _, _, _, _, _),
    length(T, L),
    (L > 0 ->
        string_lower(Title, S),
        split_string(S, " ", " ", Words),
        (member(Word, Words) -> Confirmacao is 1 ; titleContainsWordSerie(T, Movie, Conf), (Conf =:= 1 -> Confirmacao is 1 ; Confirmacao is 0));
        string_lower(Title, S),
        split_string(S, " ", " ", Words),
        (member(Word, Words) -> Confirmacao is 1 ; Confirmacao is 0)
    ).


filterMovies([Movie | T], Names, ResultTemp, Result):-
    length(T, L),
    (L > 0 ->
        titleContainsWordMovie(Names, Movie, Confirmacao),
        (Confirmacao =:= 1 ->
            filterMovies(T, Names, [Movie | ResultTemp], Result);
            filterMovies(T, Names, ResultTemp, Result)
        )
        ;
        titleContainsWordMovie(Names, Movie, Confirmacao),
        (Confirmacao =:= 1 ->
            reverse([Movie | ResultTemp], Result);
            reverse(ResultTemp, Result)
        )
    ).


searchMovie(Connection, Title, Movies):-
    split_string(Title, " ", " ", QWords),
    dbop:db_query(
        Connection,
        "SELECT * FROM movies m;",
        Result
    ),
    dbop:db_parameterized_query(
        Connection,
        "SELECT * FROM movies m WHERE m.title = '%w';",
        [Title],
        Movie
    ),
    (Movie = [] -> 
        filterMovies(Result, QWords, [], Movies); 
        Movie = [M], filterMovies(Result, QWords, [M], Movies)
    ).

registerMovie(Connection, Title, ReleaseDate, Summary, Duration, Movie):-
    dbop:db_parameterized_query_no_return(
        Connection,
        "INSERT INTO movies(title, releasedate, duration, summary) VALUES ('%w','%w','%w','%w')",
        [Title, ReleaseDate, Summary, Duration]
    ),
    dbop:db_parameterized_query(
        Connection,
        "SELECT * FROM movies WHERE title='%w';",
        [Title],
        [Movie]
    ).


addCastingToMovie(Connection, Movie, [Actor | T], Confirmacao):-
    Movie = row(MovieId, _, _, _, _, _),
    ( T = [] ->
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO casting VALUES (%w,'%w', 'actor')",
            [MovieId, Actor]
        ), Confirmacao is 1
        ;
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO casting VALUES (%w,'%w', 'actor')",
            [MovieId, Actor]
        ),
        addCastingToMovie(Connection, Movie, T, Confirmacao)
    ).


addDirectorsToMovie(Connection, Movie, [Director | T], Confirmacao):-
    Movie = row(MovieId, _, _, _, _, _),
    ( T = [] ->
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO casting VALUES (%w,'%w', 'director')",
            [MovieId, Director]
        ), Confirmacao is 1
        ;
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO casting VALUES (%w,'%w', 'director')",
            [MovieId, Director]
        ),
        addDirectorsToMovie(Connection, Movie, T, Confirmacao)
    ).


showMovieRating(Connection, Movie, Rating):-
    Movie = row(MovieId, _, _, _, _, _),
    dbop:db_parameterized_query(
        Connection,
        "SELECT SUM(rating), COUNT(rating) FROM ratings WHERE movieid=%w;",
        [MovieId],
        [Rating]
    ).


updateMovieRating(Connection, Movie, Rating):-
    Movie = row(MovieId, _, _, _, _, _),
    showMovieRating(Connection, Movie, [Sum, Count | _ ]),
    Q is round((Sum / Count) * 10) / 10,
    dbop:db_parameterized_query_no_return(
        Connection,
        "UPDATE movies SET rating = %w WHERE movieid = %w;",
        [Q, MovieId]
    ).


updateAllMovieRating(Connection, [Movie | T]):-
    (T = [] ->
        updateMovieRating(Connection, Movie, Rating);
        updateMovieRating(Connection, Movie, Rating),
        updateAllMovieRating(Connection, T)
    ).



getMoviesWithRatings(Connection, Movies):-
    dbop:db_query(
        Connection,
        "SELECT movieid, count(movieid) FROM ratings group by movieid ;",
        MoviesIds
    ), getMoviesById(Connection, MoviesIds, [], Movies).
    

getMoviesByCategory(Connection, Category, Movies):-
    dbop:db_parameterized_query(
        Connection,
        "select movieid, title, releasedate, duration, summary, rating from ((select movieid as mid, category  from categories c where category = '%w') as c join (select m.movieid, m.title, m.releasedate, m.duration, m.summary, m.rating from (movies m join (select coalesce(c1 - c2, c1) as c, mid1 as mid from (((select count(movieid) as c1, movieid as mid1 from ratings where rating > 3 group by mid1 order by c1 desc) as one left outer join (select count(movieid) as c2, movieid as mid2 from ratings where rating < 4 group by mid2 order by c2 desc) as two on one.mid1 = two.mid2)) order by c desc) r on m.movieid = r.mid) order by c desc) m on c.mid = m.movieid) order by rating desc limit 10;",
        [Category],
        Movies
    ).


getMoviesById(Connection, [MovieId | T], ResultTemp, Result):-
    (T = [] ->
        dbop:db_parameterized_query(
            Connection,
            "SELECT * FROM movies WHERE movieid = %w",
            [MovieId],
            [Movie]
        ), reverse([Movie | ResultTemp], Result);
        dbop:db_parameterized_query(
            Connection,
            "SELECT * FROM movies WHERE movieid = %w",
            [MovieId],
            [Movie]
        ), getMoviesById(Connection, T, [Movie | ResultTemp], Result)
    ).


getWatchLaterList(Connection, User, Movies):-
    User = row(Email, _, _, _),
    dbop:db_parameterized_query(
        Connection,
        "SELECT m.movieid, m.title, m.releasedate, m.duration, m.summary, m.rating FROM (watchlaterlist w JOIN movies m ON w.movieid=m.movieid) WHERE w.useremail = '%w';",
        [Email],
        Movies
    ).


getCategoriesOfMoviesInOneString(Connection, Movie, String):-
    Movie = row(MovieId, _, _, _, _, _),
    dbop:db_parameterized_query(
        Connection,
        "SELECT DISTINCT category FROM categories WHERE movieid = %w;",
        [MovieId],
        Categories
    ),
    (Categories = [] -> 
        String = "" ; 
        getCategoriesFromRow(Categories, [], Cats),
        util:concatenate_with_comma(Cats, String)
    ).



getCategoriesFromRow([Category | T], ResultTemp, Result):-
    (T = [] ->
        Category = row(X),
        reverse([X | ResultTemp], Result);
        Category = row(X),
        getCategoriesFromRow(T, [X | ResultTemp], Result)
    ).


addCategoriesToMovie(Connection, Movie, [Category | T], Confirmacao):-
    Movie = row(MovieId, _, _, _, _, _),
    (T = [] ->
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO categories VALUES (%w,'%w')",
            [MovieId, Category]
        ), Confirmacao is 1;
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO categories VALUES (%w,'%w')",
            [MovieId, Category]
        ), addCategoriesToMovie(Connection, Movie, T, Confirmacao)    
    ).


getRecomendationsOfMovies(Connection, User, Movies):-
    getUsersWhoAvaluateWell(Connection, Users),
    avaluateRecomendations(Connection, Users, User, [], Movies).



avaluateRecomendations(_, [], _, MoviesTemp, RecommendedMovies):- reverse(MoviesTemp, RecommendedMovies).
avaluateRecomendations(Connection, [User | T], UserX, MoviesTemp, RecommendedMovies):-
    (length(MoviesTemp, L), L >= 10 ->
        reverse(MoviesTemp, RecommendedMovies);
        getMoviesAvaluatedWellByUser(Connection, User, Movies),
        getMoviesAvaluatedWellByUser(Connection, UserX, MyMovies),
        intersection(Movies, MyMovies, Alike),
        (length(Alike, ALength), length(MyMovies, MyMLength), ALength > (MyMLength // 2) + 1 ->
            subtract(Movies, MyMovies, Rec),
            append(Rec, MoviesTemp, MoviesTempTemp),
            avaluateRecomendations(Connection, T, UserX, MoviesTempTemp, RecommendedMovies);
            avaluateRecomendations(Connection, T, UserX, MoviesTemp, RecommendedMovies)
        )
    ).


getAvaluations(Connection, Email, Ratings):-
    dbop:db_parameterized_query(Connection, "select * from ratings r where useremail = '%w';", [Email], Ratings).
    

getMoviesAvaluatedWellByUser(Connection, User, Movies):-
    User = row(Email, _, _, _),
    dbop:db_parameterized_query(
        Connection,
        "select movieid, rating from ratings r where useremail = '%w' and rating > 3 order by rating desc limit 10;",
        [Email],
        MoviesIdsRow
    ), getMoviesIdFromRow(MoviesIdsRow, [], MoviesIds),
    getMoviesById(Connection, MoviesIds, [], Movies).


getMoviesIdFromRow([MovieId | T], ResultTemp, Result):-
    (T = [] ->
        MovieId = row(X, _),
        reverse([X | ResultTemp], Result);
        MovieId = row(X, _),
        getEmailsFromRow(T, [X | ResultTemp], Result)
    ).


getUsersWhoAvaluateWell(Connection, Users):-
    dbop:db_query(
        Connection,
        "select distinct useremail from ratings r where rating > 3 group by useremail;",
        UserEmails
    ),
    getEmailsFromRow(UserEmails, [], Emails),
    user_operations:getUsersByEmail(Connection, Emails, [], Users).


getEmailsFromRow([UserEmail | T], ResultTemp, Result):-
    (T = [] ->
        UserEmail = row(X),
        reverse([X | ResultTemp], Result);
        UserEmail = row(X),
        getEmailsFromRow(T, [X | ResultTemp], Result)
    ).


getTenBestMovies(Connection, Movies):-
    dbop:db_query(
        Connection,
        "select m.movieid, m.title, m.releasedate, m.duration, m.summary, m.rating from (movies m left outer join (select coalesce(c1 - c2, c1) as c, mid1 as mid from (((select count(movieid) as c1, movieid as mid1 from ratings where rating > 3 group by mid1 order by c1 desc) as one left outer join (select count(movieid) as c2, movieid as mid2 from ratings where rating < 4 group by mid2 order by c2 desc) as two on one.mid1 = two.mid2)) order by c desc) r on m.movieid = r.mid) order by c desc nulls last limit 10;",
        Movies
    ).

addToWatchLaterList(Connection, User, Movie, Confirmacao):-
    User = row(Email, _, _, _),
    Movie = row(MovieId, _, _, _, _, _),
    (user_operations:userAlreadyExists(Connection, Email, 1) ->
        getWatchLaterList(Connection, User, Movies),
        getMoviesIdFromMovies(Movies, [], MoviesId),
        (member(MovieId, MoviesId) -> Confirmacao is 0;
            dbop:db_parameterized_query_no_return(
                Connection,
                "INSERT INTO watchlaterlist values ('%w',%w)",
                [Email, MovieId]
            ),
            Confirmacao is 1
        );
        Confirmacao is 0
    ).


getMoviesIdFromMovies([Movie | T], ResultTemp, Result):-
    (T = [] ->
        Movie = row(X, _, _, _, _, _),
        reverse([X | ResultTemp], Result);
        Movie = row(X, _, _, _, _, _),
        getMoviesIdFromMovies(T, [X | ResultTemp], Result)
    ).
