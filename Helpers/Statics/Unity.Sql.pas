unit Unity.Sql;

// ----------------------------------------
// Extension unit for application.
// Can be referenced by anyone.
// Cannot hold references to View or Logic.
// ----------------------------------------

interface


type


    TSql = class abstract
        const INSERT       = ' INSERT INTO ';
        const VAL          = ' VALUES ';
        const SELECT       = ' SELECT ';
        const SELECT_DIS   = ' SELECT DISTINCT ';
        const DISTINCT     = ' DISTINCT ';
        const _UPDATE      = ' UPDATE ';
        const DELETE_FROM  = ' DELETE FROM ';
        const _SET         = ' SET ';
        const _BEGIN       = ' BEGIN ';
        const _END         = ' END ';
        const UNION        = ' UNION ALL ';
        const EQUAL        = ' = ';
        const FROM         = ' FROM ';
        const ALL          = ' * ';
        const WHERE        = ' WHERE ';
        const LEFT_JOIN    = ' LEFT JOIN ';
        const RIGHT_JOIN   = ' RIGHT JOIN ';
        const INNER_JOIN   = ' INNER JOIN ';
        const OUTER_JOIN   = ' OUTER JOIN ';
        const _ON          = ' ON ';
        const _OR          = ' OR ';
        const ORDER        = ' ORDER BY ';
        const ASC          = ' ASC ';
        const DESC         = ' DESC ';
        const _AND         = ' AND ';
        const _AS          = ' AS ';
        const MAX          = ' MAX ';
        const SUM          = ' SUM ';
        const LIKE         = ' LIKE ';
        const EXECUTE      = ' EXEC ';
        const MATCHCASE    = ' COLLATE SQL_Latin1_General_CP1_CS_AS ';
    end;


implementation


end.

