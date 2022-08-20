:- module(serie_operations, [
        avaluateSerie/6,
        getSerieIdFromRatings/3,
        getRatingsSeries/3,
        getCastingSerie/3,
        titleContainsWordSerie/3,
        filterSeries/4, 
        searchSerie/3,
        registerSerie/6,
        addCastingToSerie/4,
        addDirectorsToSerie/4,
        showSerieRating/3,
        updateSerieRating/3,
        updateAllSerieRating/2,
        getSeriesWithRatings/2,
        getSeriesByCategory/3,
        getSeriesById/4,
        getWatchLaterListSeries/3,
        getCategoriesOfSeriesInOneString/3,
        getCategoriesFromRowSeries/3,
        addCategoriesToSeries/4,
        getRecomendationsOfSeries/3,
        avaluateRecomendationsSeries/5,
        getAvaluationsSeries/3,
        getSeriesAvaluatedWellByUser/3,
        getSeriesIdFromRow/3,
        getUsersWhoAvaluateWellSeries/2,
        getEmailsFromRowSeries/3,
        getTenBestSeries/2,
        addToWatchLaterListSeries/4,
        getSeriesIdFromSeries/3
    ]).
:- use_module("./util.pl").
:- use_module("./user_operations.pl").
:- use_module("../Db/dbOperations.pl").


avaluateSerie(Connection, User, Serie, Rating, Commentary, Confirmacao):-
    User = row(Email, Senha, Nome, Sobrenome),
    user_operations:userAlreadyExists(Connection, Email, Fstconf),
    getAvaluationsSeries(Connection, Email, Ratings),
    getSerieIdFromRatings(Ratings, [], SeriesIds),
    (Fstconf =:= 1, member(Rating, [1, 2, 3, 4, 5]), Serie = row(SerieId, _, _, _, _, _), not(member(SerieId, SeriesIds)) -> 
        dbop:db_parameterized_query_no_return(
            Connection,
            "INSERT INTO seriesratings (useremail, serieid, rating, commentary) values ('%w', %w, %w, '%w')",
            [Email, SerieId, Rating, Commentary]
        ),
        Confirmacao is 1;
        Confirmacao is 0
    ).


getSerieIdFromRatings([], ResultTemp, Result):- reverse(ResultTemp, Result).
getSerieIdFromRatings([Rating|T], ResultTemp, Result):- 
    Rating = row(_, _, SerieId, _, _), 
    getSerieIdFromRatings(T, [SerieId|ResultTemp], Result).



getRatingsSeries(Connection, Serie, Ratings):-
    Serie = row(SerieId, _, _, _, _, _),
    dbop:db_parameterized_query(Connection, "SELECT * FROM seriesratings WHERE serieid = %w;", [SerieId], Ratings).

    
getCastingSerie(Connection, Serie, Casting):-
    Serie = row(SerieId, _, _, _, _, _),
    dbop:db_parameterized_query(Connection, "SELECT name, seriefunction FROM seriescasting WHERE seriesid = %w;", [SerieId], Casting).


titleContainsWordSerie([], Serie, Confirmacao):- Confirmacao is 0.
titleContainsWordSerie([Word|T], Serie, Confirmacao):-
    Serie = row(_, Title, _, _, _, _),
    length(T, L),
        (L > 0 ->
            string_lower(Title, S),
            split_string(S, " ", " ", Words),
            (member(Word, Words) -> 
                Confirmacao is 1 ; titleContainsWordSerie(T, Serie, Conf), 
                (Conf =:= 1 -> Confirmacao is 1 ; Confirmacao is 0)
            );
            string_lower(Title, S),
            split_string(S, " ", " ", Words),
            (member(Word, Words) -> Confirmacao is 1 ; Confirmacao is 0)
        ).


filterSeries([], Names, ResultTemp, Result):- reverse(ResultTemp, Result).
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
        "INSERT INTO series(title, releasedate, episodes, summary) VALUES ('%w','%w','%w','%w')",
        [Title, ReleaseDate, Summary, Episodes]
    ),
    db_parameterized_query(
        Connection,
        "SELECT * FROM series WHERE title='%w';",
        [Title],
        [Serie]
    ).

addCastingToSerie(Connection, Serie, [], Confirmacao):- Confirmacao is 1.
addCastingToSerie(Connection, Serie, [Actor | T], Confirmacao):-
    Serie = row(SerieId, _, _, _, _, _),
    
    dbop:db_parameterized_query_no_return(
        Connection,
        "INSERT INTO seriescasting VALUES (%w,'%w', 'actor')",
        [SerieId, Actor]
    ),
    addCastingToSerie(Connection, Serie, T, Confirmacao).


addDirectorsToSerie(Connection, Serie, [], Confirmacao):- Confirmacao is 1.
addDirectorsToSerie(Connection, Serie, [Director | T], Confirmacao):-
    Serie = row(SerieId, _, _, _, _, _),
    dbop:db_parameterized_query_no_return(
        Connection,
        "INSERT INTO seriescasting VALUES (%w,'%w', 'director')",
        [SerieId, Director]
    ),
    addDirectorsToSerie(Connection, Serie, T, Confirmacao).


showSerieRating(Connection, Serie, Rating):-
    Serie = row(SerieId, _, _, _, _, _),
    dbop:db_parameterized_query(Connection, "SELECT COALESCE(SUM(rating), 0), COUNT(rating) FROM seriesratings WHERE serieid= %w;", [SerieId], [Rating]).


updateSerieRating(Connection, Serie, Rating):-
    Serie = row(SerieId, _, _, _, _, _),
    showSerieRating(Connection, Serie, Rat),
    Rat = row(S, C),
    A is ((S / C) * 10),
    B is round(A),
    Rating is B / 10,
    dbop:db_parameterized_query_no_return(
        Connection,
        "UPDATE series SET rating = %w WHERE seriesid = %w;",
        [Rating, SerieId]
    ).


updateAllSerieRating(Connection, []).
updateAllSerieRating(Connection, [Serie | T]):-
    updateSerieRating(Connection, Serie, Rating),
    updateAllSerieRating(Connection, T).


getSeriesWithRatings(Connection, Serie):-
    dbop:db_query(
        Connection,
        "SELECT serieid, count(serieid) FROM seriesratings group by serieid ;",
        SerieIds
    ), getSeriesById(Connection, SerieIds, [], Serie).


getSeriesByCategory(Connection, Category, Series):-
    dbop:db_parameterized_query(
        Connection,
        "select seriesid, title, releasedate, episodes, summary, rating from ((select seriesid as sid, category from seriescategories c where category = '%w') as c join (select s.seriesid, s.title, s.releasedate, s.episodes, s.summary, s.rating from (series s left outer join (select coalesce(c1 - c2, c1) as c, sid1 as sid from (((select count(serieid) as c1, serieid as sid1 from seriesratings where rating > 3 group by sid1 order by c1 desc) as one left outer join (select count(serieid) as c2, serieid as sid2 from seriesratings where rating < 4 group by sid2 order by c2 desc) as two on one.sid1 = two.sid2)) order by c desc) r on s.seriesid = r.sid) order by c desc nulls last) s on c.sid = s.seriesid) order by rating desc limit 10;",
        [Category],
        Series
    ).

getSeriesById(Connection, [], ResultTemp, Result):- reverse(ResultTemp, Result).
getSeriesById(Connection, [ row(SerieId, _) | T], ResultTemp, Result):-
    dbop:db_parameterized_query(
        Connection,
        "SELECT * FROM series WHERE seriesid = %w",
        [SerieId],
        [Serie]
    ), getSeriesById(Connection, T, [Serie | ResultTemp], Result).
    


getWatchLaterListSeries(Connection, User, Series):-
    User = row(Email, _, _, _),
    dbop:db_parameterized_query(
        Connection,
        "SELECT s.seriesid, s.title, s.releasedate, s.episodes, s.summary, s.rating FROM (watchlaterlistseries w JOIN series s ON w.serieid=s.seriesid) WHERE w.useremail = '%w';",
        [Email],
        Series
    ).


getCategoriesOfSeriesInOneString(Connection, Serie, String):-
    Serie = row(SerieId, _, _, _, _, _),
    dbop:db_parameterized_query(
        Connection,
        "SELECT DISTINCT category FROM seriescategories WHERE seriesid = %w;",
        [SerieId],
        Categories
    ),
    (Categories = [] -> 
        String = "" ; 
        getCategoriesFromRowSeries(Categories, [], Cats),
        util:concatenate_with_comma(Cats, String)
    ).

getCategoriesFromRowSeries([], ResultTemp, Result):- reverse(ResultTemp, Result).
getCategoriesFromRowSeries([Category | T], ResultTemp, Result):-
    Category = row(X),
    getCategoriesFromRowSeries(T, [X | ResultTemp], Result).


addCategoriesToSeries(Connection, Series, [], Confirmacao):- Confirmacao is 1.
addCategoriesToSeries(Connection, Series, [Category | T], Confirmacao):-
    Series = row(SeriesId, _, _, _, _, _),
    dbop:db_parameterized_query_no_return(
        Connection,
        "INSERT INTO seriescategories VALUES (%w,'%w')",
        [SeriesId, Category]
    ), addCategoriesToSeries(Connection, Series, T, Confirmacao).
    


getRecomendationsOfSeries(Connection, User, Seriess):-
    getUsersWhoAvaluateWellSeries(Connection, Users),
    avaluateRecomendationsSeries(Connection, Users, User, [], Seriess).


avaluateRecomendationsSeries(_, [], _, SeriesTemp, RecommendedSeries):- reverse(SeriesTemp, RecommendedSeries).
avaluateRecomendationsSeries(Connection, [User | T], UserX, SeriesTemp, RecommendedSeries):-
    (length(SeriesTemp, L), L >= 10 ->
        reverse(SeriesTemp, RecommendedSeries);
        getSeriesAvaluatedWellByUser(Connection, User, Series),
        getSeriesAvaluatedWellByUser(Connection, UserX, MySeries),
        intersection(Series, MySeries, Alike),
        length(Alike, ALength), length(MySeries, MyMLength), 
        Length is (MyMLength // 2) + 1,
        (ALength > Length ->
            subtract(Series, MySeries, Rec),
            append(Rec, SeriesTemp, SeriesTempTemp),
            avaluateRecomendationsSeries(Connection, T, UserX, SeriesTempTemp, RecommendedSeries);
            avaluateRecomendationsSeries(Connection, T, UserX, SeriesTemp, RecommendedSeries)
        )
    ).

getAvaluationsSeries(Connection, Email, Ratings):-
    dbop:db_parameterized_query(Connection, "select * from seriesratings r where useremail = '%w';", [Email], Ratings).


getSeriesAvaluatedWellByUser(Connection, User, Series):-
    User = row(Email, _, _, _),
    dbop:db_parameterized_query(
        Connection,
        "select serieid, rating from seriesratings r where useremail = '%w' and rating > 3 order by rating desc limit 10;",
        [Email],
        SeriesIdsRow
    ), getSeriesIdFromRow(SeriesIdsRow, [], SeriesIds),
    getSeriesById(Connection, SeriesIds, [], Series).


getSeriesIdFromRow([], ResultTemp, Result):- reverse(ResultTemp, Result).
getSeriesIdFromRow([SerieId | T], ResultTemp, Result):-
    SerieId = row(X, _),
    getEmailsFromRowSeries(T, [X | ResultTemp], Result).
    

getUsersWhoAvaluateWellSeries(Connection, Users):-
    dbop:db_query(
        Connection,
        "select distinct useremail from seriesratings r where rating > 3 group by useremail;",
        UserEmails
    ),
    getEmailsFromRowSeries(UserEmails, [], Emails),
    user_operations:getUsersByEmail(Connection, Emails, [], Users).


getEmailsFromRowSeries([], ResultTemp, Result):- reverse(ResultTemp, Result).
getEmailsFromRowSeries([UserEmail | T], ResultTemp, Result):-
    UserEmail = row(X),
    getEmailsFromRowSeries(T, [X | ResultTemp], Result).


getTenBestSeries(Connection, Series):-
    dbop:db_query(
        Connection,
        "select s.seriesid, s.title, s.releasedate, s.episodes, s.summary, s.rating from (series s left outer join (select coalesce(c1 - c2, c1) as c, sid1 as sid from (((select count(serieid) as c1, serieid as sid1 from seriesratings where rating > 3 group by sid1 order by c1 desc) as one left outer join (select count(serieid) as c2, serieid as sid2 from seriesratings where rating < 4 group by sid2 order by c2 desc) as two on one.sid1 = two.sid2)) order by c desc) r on s.seriesid = r.sid) order by c desc nulls last limit 10;",
        Series
    ).


addToWatchLaterListSeries(Connection, User, Serie, Confirmacao):-
    User = row(Email, _, _, _),
    Serie = row(SerieId, _, _, _, _, _),
    (user_operations:userAlreadyExists(Connection, Email, 1) ->
        getWatchLaterListSeries(Connection, User, Series),
        getSeriesIdFromSeries(Series, [], SeriesId),
        (member(SerieId, SeriesId) -> Confirmacao is 0;
            dbop:db_parameterized_query_no_return(
                Connection,
                "INSERT INTO watchlaterlistseries values ('%w',%w)",
                [Email, SerieId]
            ),
            Confirmacao is 1
        );
        Confirmacao is 0
    ).

getSeriesIdFromSeries([], ResultTemp, Result):- reverse(ResultTemp, Result).
getSeriesIdFromSeries([Serie | T], ResultTemp, Result):-
    Serie = row(X, _, _, _, _, _),
    getSeriesIdFromSeries(T, [X | ResultTemp], Result).