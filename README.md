# mapr-auth API

API to interact with authorization components of a MapR cluster including MapR Access Control Expressions (ACES), MapR Roles, and Hive SQL Authorization

This first iteration supports MapR ACES only. This is a work in progress.


## Install

    mvn clean install -PcheckstyleSkip

## Run the app

    java -cp <path-to-dependecnies> com.mapr.auth.server.MapRAuthServer

## Run the tests

    mvn clean install -DskipTests=false -PcheckstyleSkip
    


# REST API

The REST API for mapr-auth is described below

## Get list of ACES

### Request

`GET /api/v1/aces`

    curl -i -H 'Accept: application/json' http://localhost:8005/api/v1/aces/
    

### Response

    HTTP/1.1 200 OK
    Date: Thu, 24 Feb 2011 12:36:30 GMT
    Status: 200 OK
    Connection: close
    Content-Type: application/json
    Content-Length: 2

```json
   [
     {
       "id": 1,
       "name": "my_ace",
       "status": "enabled",
       "ace": "(u:user1 & !g:group1) & (u:user2 & g:group2)"
     },
     {
       "id": 1,
       "name": "my_ace",
       "status": "enabled",
       "ace": "(u:user3 & !g:group3) & (u:user4 & g:group4)"
     }
]
```


## Create a new ACE

### Request

`PUT /api/v1/aces/`

    curl -i -H 'Accept: application/json' -d @FILENAME http://localhost:8005/api/v1/aces
    
### Body
```json
 {
  "ace": {
    "name": "my_ace",
    "access": {
      "path": "/users/mypath",
      "type": "READDIR"
    }
  },
  "expressions": {
    "expression": [
      {
        "groupName": "group1",
        "groupOperator": "&",
        "operation": "&",
        "order": 0,
        "type": "u",
        "value": "user1"
      },
      {
        "groupName": "group1",
        "groupOperator": "&",
        "operation": "!",
        "order": 1,
        "type": "g",
        "value": "group1"
      },
      {
        "groupName": "group2",
        "operation": "&",
        "order": 0,
        "type": "u",
        "value": "user2"
      },
      {
        "groupName": "group2",
        "order": 1,
        "type": "g",
        "value": "group2"
      }
    ]
  }
} 
```

### Response

    HTTP/1.1 201 Created
    Date: Thu, 24 Feb 2011 12:36:30 GMT
    Status: 201 Created
    Connection: close
    Content-Type: application/json
    Location: /thing/1
    Content-Length: 36    
    
 ```json
      {
        "id": 1,
        "name": "my_ace",
        "status": "created",
        "ace": "(u:user3 & !g:group3) & (u:user4 & g:group4)"
      }
 ```    


## Get a specific ACE

### Request

`GET /api/v1/ace/:id`

    curl -i -H 'Accept: application/json' http://localhost:8005/api/v1/aces/my_ace

### Response

    HTTP/1.1 200 OK
    Date: Thu, 24 Feb 2011 12:36:30 GMT
    Status: 200 OK
    Connection: close
    Content-Type: application/json
    Content-Length: 36
    
 ```json
      {
        "id": 1,
        "name": "my_ace",
        "status": "enabled",
        "ace": "(u:user3 & !g:group3) & (u:user4 & g:group4)"
      }
 ```    


## Change an ACE

### Request

`POST /api/v1/ace/:id`

    curl -i -H 'Accept: application/json' -X PUT -d @FILENAME http://localhost:8005/api/v1/aces/1

### Response

    HTTP/1.1 200 OK
    Date: Thu, 24 Feb 2011 12:36:31 GMT
    Status: 200 OK
    Connection: close
    Content-Type: application/json
    Content-Length: 41

 ```json
      {
        "id": 1,
        "name": "my_ace",
        "status": "modified",
        "ace": "(u:user3 & !g:group3) & (u:user4 & g:group4)"
      }
 ```



## Delete an ACE

### Request

`DELETE /api/v1/ace/id`

    curl -i -H 'Accept: application/json' -X DELETE http://localhost:8005/api/v1/aces/1/

### Response

    HTTP/1.1 204 No Content
    Date: Thu, 24 Feb 2011 12:36:32 GMT
    Status: 204 No Content
    Connection: close
    
```json
      {
        "id": 1,
        "name": "my_ace",
        "status": "deleted",
        "ace": "(u:user3 & !g:group3) & (u:user4 & g:group4)"
      }
```      


# Java API

The mapr-auth API can be accessed directly from java code. The API leverages the builder pattern to create a MapR ACE object. For example, the following snippet...

```java
AceExpression a = new AceExpression("u", "user1", AceOperator.AND.get(), "g1", AceOperator.AND.get(), 0);
AceExpression b = new AceExpression("g", "group1", AceOperator.NOT.get(), "g1", AceOperator.AND.get(), 1);
AceExpression c = new AceExpression("u", "user2", AceOperator.AND.get(), "g2", null, 0);
AceExpression d = new AceExpression("g", "group2", null, "g2", null, 1);

List<AceExpression> expressions = Arrays.asList(a, b, c, d);

MaprAce result = new MaprAceBuilder().with($ -> {
	$.name = "my_ace";
	$.access = new AceAccessType(System.currentTimeMillis(), MapRFileAce.AccessType.READDIR);
	$.expressions = expressions;
}).build();
```

would produce this ACE expression...

(u:user1 & !g:group1) & (u:user2 & g:group2) 
