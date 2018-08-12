#!/bin/bash




ovsdb-client transact tcp:127.0.0.1:6640  '[
    "simple",
    {
        "op": "delete",
        "table": "Item",
        "where": [] 
    },
    {
        "op": "delete",
        "table": "List",
        "where": [] 
    }
]'

ovsdb-client transact tcp:127.0.0.1:6640 '["simple",{"op":"insert", "table":"Item", "row":{"description":"Sample Item1", "status": "new"}, "uuid-name":"newitem" }, {"op":"insert", "table":"List", "row":{"name":"List2", "items":["set",[["named-uuid","newitem"]]]}}]'

ovsdb-client transact tcp:127.0.0.1:6640 '["simple",{"op":"insert", "table":"Item", "row":{"description":"Sample Item2", "status": "new"}, "uuid-name":"newitem" }, {"op":"insert", "table":"List", "row":{"name":"List3", "items":["set",[["named-uuid","newitem"]]]}}]'

ovsdb-client transact tcp:127.0.0.1:6640  '[
    "simple",
    {
        "op": "insert",
        "table": "Item",
        "row": {
            "description": "Sample Item3",
            "status": "new"
        },
        "uuid-name": "anotheritem"
    },
    {
        "op": "mutate",
        "table": "List",
        "where": [
            ["name", "==", "List2"]
        ],
        "mutations": [
            [
                "items",
                "insert",
                [
                    "set",
                    [
                        ["named-uuid", "anotheritem"]
                    ]
                ]
            ]
        ]
    }
]'

while true;
do
ovsdb-client transact tcp:127.0.0.1:6640  '[
    "simple",
    {
        "op": "update",
        "table": "Item",
        "where": [
            ["description", "==", "Sample Item3"]
        ],

        "row": {
            "status": "old"
        }
    }
]'

sleep 2
ovsdb-client transact tcp:127.0.0.1:6640  '[
    "simple",
    {
        "op": "update",
        "table": "Item",
        "where": [
            ["description", "==", "Sample Item3"]
        ],

        "row": {
            "status": "new"
        }
    }
]'

sleep 2

done
