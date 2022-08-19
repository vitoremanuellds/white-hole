:- module(serie_operations, []).

:- use_module("./util.pl").
:- use_module("./user_operations.pl").
:- use_module("../Db/dbOperations.pl").


avaluateSerie(Connection, User, Serie, Rating, Commentary, Confirmacao):-
    User = row(Email, Senha, Nome, Sobrenome),
    user_operations:userAlredyExists(Connection, Email, Fstconf),
    getAvaluationsSeries(Connection, Email, Ratings),
    getAvaluationsSeries(Ratings, [], SeriesIds),
    (Fstconf =:= 1, member(Rating, [1, 2, 3, 4, 5]), Serie = row(SerieId, _, _, _, _, _), not(member(SerieId, SeriesIds)) -> 
        dboc:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO seriesratings (useremail, serieid, rating, commentary) values ('%w', %w, %w, '%w')",
            [Email, SerieId, Rating, Commentary]
        ),
        Confirmacao is 1;
        Confirmacao is 0
    ).


getSerieIdFromRatings([Rating|T], ResultTemp, Result):-
    length(T, L),
    (L > 0 ->
        Rating = row(_, _, SerieId, _, _), 
        getSerieIdFromRatings(T, [SerieId|ResultTemp], Result);
        Rating = row(_, _, SerieId, _, _),
        reverse([ SerieId | ResultTemp ], Result)
    ).

getRatingsSeries(Connection, Serie, Ratings):-
    Serie = row(SerieId, _, _, _, _, _),
    dboc:db_parameterized_query(Connection, "SELECT * FROM seriesratings WHERE serieid = %w;", [SerieId], Ratings).

    
getCastingSerie(Connection, Serie, Casting):-
    Serie = row(SerieId, _, _, _, _, _),
    dboc:db_parameterized_query(Connection, "SELECT name, seriefunction FROM seriescasting WHERE seriesid = %w;", [SerieId], Casting).


titleContainsWordSerie([Word|T], Serie, Confirmacao):-
    Serie = row(_, Title, _, _, _, _),
    length(T, L),
        (L > 0 ->
            string_lower(Title, S),
            split_string(S, " ", " ", Words),
            (member(Word, Words) -> Confirmacao is 1 ; titleContainsWordSerie(T, Serie, Conf), (Conf =:= 1 -> Confirmacao is 1 ; Confirmacao is 0));
            string_lower(Title, S),
            split_string(S, " ", " ", Words),
            (member(Word, Words) -> Confirmacao is 1 ; Confirmacao is 0)
        ).


filterSeries([Serie | T], Names, ResultTemp, Result):-
    length(T, L),
    (L > 0 ->
        titleContainsWordSerie(Names, Serie, Confirmacao),
        (Confirmacao =:= 1 ->
            filterSeries(T, Names, [Serie | ResultTemp], Result);
            filterSeries(T, Names, ResultTemp, Result)
        )
        ;
        titleContainsWordSerie(Names, Serie, Confirmacao),
        (Confirmacao =:= 1 ->
            reverse([Serie | ResultTemp], Result);
            reverse(ResultTemp, Result)
        )
    ).

searchSerie(Connection, Title, Series):-
    split_string(Title, " ", " ", QWords),
    dbop:db_query(
        Connection,
        "SELECT * FROM series s;",
        Result
    ),
    dbop:db_parameterized_query(
        Connection,
        "SELECT * FROM series s WHERE s.title = '%w';",
        [Title],
        Serie
    ),
    (Serie = [] -> 
        filterSeries(Result, QWords, [], Series); 
        Serie = [S], filterSeries(Result, QWords, [S], Series)
    ).


registerSerie(Connection, Title, ReleaseDate, Summary, Episodes, Serie):-
    db_parameterized_query_no_return(
        Connection,
        "INSERT INTO series(title, releasedate, Episodes, summary) VALUES ('%w','%w','%w','%w')",
        [Title, ReleaseDate, Summary, Episodes]
    ),
    db_parameterized_query(
        Connection,
        "SELECT * FROM series WHERE title='%w';",
        [Title],
        [Serie]
    ).


addCastingToSerie(Connection, Serie, [Actor | T], Confirmacao):-
    Serie = row(SerieId, _, _, _, _, _),
    ( T = [] ->
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO seriescasting VALUES (%w,'%w', 'actor')",
            [SerieId, Actor]
        ), Confirmacao is 1
        ;
        dbop:db_parameterized_query_noreturn(
            Connection,
            "INSERT INTO seriescasting VALUES (%w,'%w', 'actor')",
            [SerieId, Actor]
        ),
        addCastingToSerie(Connection, Serie, T, Confirmacao)
    ).


addDirectorsToSerie(Connection, Serie, [Director | T], Confirmacao):-
    Serie = row(SerieId, _, _, _, _, _),
    ( T = [] ->
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO seriescasting VALUES (%w,'%w', 'director')",
            [SerieId, Director]
        ), Confirmacao is 1
        ;
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO seriescasting VALUES (%w,'%w', 'director')",
            [SerieId, Director]
        ),
        addDirectorsToSerie(Connection, Serie, T, Confirmacao)
    ).


showSerieRating(Connection, Serie, Rating):-
    Serie = row(SerieId, _, _, _, _, _),
    dboc:db_parameterized_query(Connection, "SELECT SUM(rating), COUNT(rating) FROM seriesratings WHERE serieid= %w;", [SerieId], [Rating]).


updateSerieRating(Connection, Serie, Rating):-
    Serie = row(SerieId, _, _, _, _, _),
    showSerieRating(Connection, Serie, [Sum, Count | _ ]),
    Q is round((Sum / Count) * 10) / 10,
    dbop:db_parameterized_query_no_return(
        Connection,
        "UPDATE series SET rating = %w WHERE serieid = %w;",
        [Q, SerieId]
    ).

updateAllSerieRating(Connection, [Serie | T]):-
    (T = [] ->
        updateSerieRating(Connection, Serie, Rating);
        updateSerieRating(Connection, Serie, Rating),
        updateAllSerieRating(Connection, T)
    ).


getSeriesWithRatings(Connection, Serie):-
    dbop:db_query(
        Connection,
        "SELECT serieid, count(serieid) FROM seriesratings group by serieid ;",
        SerieIds
    ), getSerieById(Connection, SerieIds, [], Serie).


getSeriesByCategory(Connection, Category, Series):-
    dbop:db_parameterized_query(
        Connection,
        "select seriesid, title, releasedate, episodes, summary, rating from ((select seriesid as sid, category from seriescategories c where category = '%w') as c join (select s.seriesid, s.title, s.releasedate, s.episodes, s.summary, s.rating from (series s left outer join (select coalesce(c1 - c2, c1) as c, sid1 as sid from (((select count(serieid) as c1, serieid as sid1 from seriesratings where rating > 3 group by sid1 order by c1 desc) as one left outer join (select count(serieid) as c2, serieid as sid2 from seriesratings where rating < 4 group by sid2 order by c2 desc) as two on one.sid1 = two.sid2)) order by c desc) r on s.seriesid = r.sid) order by c desc nulls last) s on c.sid = s.seriesid) order by rating desc limit 10;",
        [Category],
        Series
    ).


getSeriesById(Connection, [SerieId | T], ResultTemp, Result):-
    (T = [] ->
        dbop:db_parameterized_query(
            Connection,
            "SELECT * FROM series WHERE serieid = %w",
            [SerieId],
            [Serie]
        ), reverse([Serie | ResultTemp], Result);
        dbop:db_parameterized_query(
            Connection,
            "SELECT * FROM series WHERE serieid = %w",
            [SerieId],
            [Serie]
        ), getSeriesById(Connection, T, [Serie | ResultTemp], Result)
    ).


getWatchLaterList(Connection, User, Series):-
    User = row(Email, _, _, _),
    dbop:db_parameterizedquery(
        Connection,
        "SELECT s.serieid, s.title, s.releasedate, s.duration, s.summary, s.rating FROM (watchlaterlist w JOIN Series s ON w.serieid=s.serieid) WHERE w.useremail = '%w';",
        [Email],
        Series
    ).


getCategoriesOfSeriesInOneString(Connection, Serie, String):-
    Serie = row(SerieId, _, _, _, _, _),
    dbop:db_parameterized_query(
        Connection,
        "SELECT DISTINCT category FROM seriescategories WHERE serieid = %w;",
        [SerieId],
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


addCategoriesToSeries(Connection, Series, [Category | T], Confirmacao):-
    Series = row(SeriesId, _, _, _, _, _),
    (T = [] ->
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO seriescategories VALUES (%w,'%w')",
            [SeriesId, Category]
        ), Confirmacao is 1;
        dbop:db_parameterized_query_noreturn(
            Connection,
            "INSERT INTO seriescategories VALUES (%w,'%w')",
            [SeriesId, Category]
        ), addCategoriesToSeries(Connection, Series, T, Confirmacao)
    ).


getRecomendationsOfSeriess(Connection, User, Seriess):-
    getUsersWhoAvaluateWell(Connection, Users),
    avaluateRecomendations(Connection, Users, User, [], Seriess).


avaluateRecomendations(_, [], _, SeriesTemp, RecommendedSeries):- reverse(SeriesTemp, RecommendedSeries).
avaluateRecomendations(Connection, [User | T], UserX, SeriesTemp, RecommendedSeries):-
    (length(SeriesTemp, L), L >= 10 ->
        reverse(SeriesTemp, RecommendedSeries);
        getSeriesAvaluatedWellByUser(Connection, User, Series),
        getSeriesAvaluatedWellByUser(Connection, UserX, MySeries),
        intersection(Series, MySeries, Alike),
        (length(Alike, ALength), length(MySeries, MyMLength), ALength > (MyMLength // 2) + 1 ->
            subtract(Series, MySeries, Rec),
            append(Rec, SeriesTemp, SeriesTempTemp),
            avaluateRecomendations(Connection, T, UserX, SeriesTempTemp, RecommendedSeries);
            avaluateRecomendations(Connection, T, UserX, SeriesTemp, RecommendedSeries)
        )
    ).

getAvaluations(Connection, Email, Ratings):-
    dboc:db_parameterizedquery(Connection, "select * from seriesratings r where useremail = '%w';", [Email], Ratings).


getSeriesAvaluatedWellByUser(Connection, User, Series):-
    User = row(Email, _, _, _),
    dbop:db_parameterizedquery(
        Connection,
        "select serieid, rating from seriesratings r where useremail = '%w' and rating > 3 order by rating desc limit 10;",
        [Email],
        SeriesIdsRow
    ), getSeriesIdFromRow(SeriesIdsRow, [], SeriesIds),
    getSeriesById(Connection, SeriesIds, [], Series).


getSeriesIdFromRow([SerieId | T], ResultTemp, Result):-
    (T = [] ->
        SerieId = row(X, _),
        reverse([X | ResultTemp], Result);
        SerieId = row(X, _),
        getEmailsFromRow(T, [X | ResultTemp], Result)
    ).


getUsersWhoAvaluateWell(Connection, Users):-
    dbop:db_query(
        Connection,
        "select distinct useremail from seriesratings r where rating > 3 group by useremail;",
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

getTenBestSeries(Connection, Series):-
    dbop:dbquery(
        Connection,
        "select s.serieid, s.title, s.releasedate, s.duration, s.summary, s.rating from (series s left outer join (select coalesce(c1 - c2, c1) as c, mid1 as mid from (((select count(seriesid) as c1, seriesid as mid1 from seriesratings where rating > 3 group by mid1 order by c1 desc) as one left outer join (select count(serieid) as c2, serieid as mid2 from seriesratings where rating < 4 group by mid2 order by c2 desc) as two on one.mid1 = two.mid2)) order by c desc) r on s.serieid = r.mid) order by c desc nulls last limit 10;",
        Series
    ).


addToWatchLaterList(Connection, User, Serie, Confirmacao):-
    User = row(Email, _, _, _),
    Serie = row(SerieId, _, _, _, _, _),
    (user_operations:userAlreadyExists(Connection, Email, 1) ->
        getWatchLaterList(Connection, User, Series),
        getSeriesIdFromSeries(Series, [], SeriesId),
        (member(SerieId, SeriesId) -> Confirmacao is 0;
            dbop:db_parameterized_query_noreturn(
                Connection,
                "INSERT INTO watchlaterlistseries values ('%w',%w)",
                [Email, SerieId]
            ),
            Confirmacao is 1
        );
        Confirmacao is 0
    ).

getSeriesIdFromSeries([Serie | T], ResultTemp, Result):-
    (T = [] ->
        Serie = row(X, _, _, _, _, _),
        reverse([X | ResultTemp], Result);
        Serie = row(X, _, _, _, _, _),
        getSeriesIdFromSeries(T, [X | ResultTemp], Result)
    ).