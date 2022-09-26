{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module OTel.API.Trace.Core.Attributes where

import Data.Int (Int64)
import Data.Text (Text)
import OTel.API.Common (AttrVals)
import OTel.API.Common.Internal (Key(..), SchemaURL(..), ToAttrVal(..))
import Prelude (Bool, Double)

-- | The URL of the OpenTelemetry schema for these keys and values.
pattern TRACE_SCHEMA_URL :: SchemaURL
pattern TRACE_SCHEMA_URL <- SchemaURL "https://opentelemetry.io/schemas/1.12.0" where
  TRACE_SCHEMA_URL = SchemaURL "https://opentelemetry.io/schemas/1.12.0"

{-|
The full invoked ARN as provided on the @Context@ passed to the function (@Lambda-Runtime-Invoked-Function-Arn@ header on the @\/runtime\/invocation\/next@ applicable).

[Notes]: This may be different from @faas.id@ if an alias is involved.
-}
pattern AWS_LAMBDA_INVOKED_ARN :: Key Text
pattern AWS_LAMBDA_INVOKED_ARN <- Key "aws.lambda.invoked_arn" where
  AWS_LAMBDA_INVOKED_ARN = Key "aws.lambda.invoked_arn"

{-|
The <https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#id> uniquely identifies the event.
-}
pattern CLOUDEVENTS_EVENT_ID :: Key Text
pattern CLOUDEVENTS_EVENT_ID <- Key "cloudevents.event_id" where
  CLOUDEVENTS_EVENT_ID = Key "cloudevents.event_id"

{-|
The <https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#source-1> identifies the context in which an event happened.
-}
pattern CLOUDEVENTS_EVENT_SOURCE :: Key Text
pattern CLOUDEVENTS_EVENT_SOURCE <- Key "cloudevents.event_source" where
  CLOUDEVENTS_EVENT_SOURCE = Key "cloudevents.event_source"

{-|
The <https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#specversion> which the event uses.
-}
pattern CLOUDEVENTS_EVENT_SPEC_VERSION :: Key Text
pattern CLOUDEVENTS_EVENT_SPEC_VERSION <- Key "cloudevents.event_spec_version" where
  CLOUDEVENTS_EVENT_SPEC_VERSION = Key "cloudevents.event_spec_version"

{-|
The <https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#type> contains a value describing the type of event related to the originating occurrence.
-}
pattern CLOUDEVENTS_EVENT_TYPE :: Key Text
pattern CLOUDEVENTS_EVENT_TYPE <- Key "cloudevents.event_type" where
  CLOUDEVENTS_EVENT_TYPE = Key "cloudevents.event_type"

{-|
The <https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#subject> of the event in the context of the event producer (identified by source).
-}
pattern CLOUDEVENTS_EVENT_SUBJECT :: Key Text
pattern CLOUDEVENTS_EVENT_SUBJECT <- Key "cloudevents.event_subject" where
  CLOUDEVENTS_EVENT_SUBJECT = Key "cloudevents.event_subject"

{-|
Parent-child Reference type

[Notes]: The causal relationship between a child Span and a parent Span.
-}
pattern OPENTRACING_REF_TYPE :: Key Text
pattern OPENTRACING_REF_TYPE <- Key "opentracing.ref_type" where
  OPENTRACING_REF_TYPE = Key "opentracing.ref_type"

{-|
An identifier for the database management system (DBMS) product being used. See below for a list of well-known identifiers.
-}
pattern DB_SYSTEM :: Key Text
pattern DB_SYSTEM <- Key "db.system" where
  DB_SYSTEM = Key "db.system"

{-|
The connection string used to connect to the database. It is recommended to remove embedded credentials.
-}
pattern DB_CONNECTION_STRING :: Key Text
pattern DB_CONNECTION_STRING <- Key "db.connection_string" where
  DB_CONNECTION_STRING = Key "db.connection_string"

{-|
Username for accessing the database.
-}
pattern DB_USER :: Key Text
pattern DB_USER <- Key "db.user" where
  DB_USER = Key "db.user"

{-|
The fully-qualified class name of the <https://docs.oracle.com/javase/8/docs/technotes/guides/jdbc/> driver used to connect.
-}
pattern DB_JDBC_DRIVER_CLASSNAME :: Key Text
pattern DB_JDBC_DRIVER_CLASSNAME <- Key "db.jdbc.driver_classname" where
  DB_JDBC_DRIVER_CLASSNAME = Key "db.jdbc.driver_classname"

{-|
This attribute is used to report the name of the database being accessed. For commands that switch the database, this should be set to the target database (even if the command fails).

[Notes]: In some SQL databases, the database name to be used is called &quot;schema name&quot;. In case there are multiple layers that could be considered for database name (e.g. Oracle instance name and schema name), the database name to be used is the more specific layer (e.g. Oracle schema name).
-}
pattern DB_NAME :: Key Text
pattern DB_NAME <- Key "db.name" where
  DB_NAME = Key "db.name"

{-|
The database statement being executed.

[Notes]: The value may be sanitized to exclude sensitive information.
-}
pattern DB_STATEMENT :: Key Text
pattern DB_STATEMENT <- Key "db.statement" where
  DB_STATEMENT = Key "db.statement"

{-|
The name of the operation being executed, e.g. the <https://docs.mongodb.com/manual/reference/command/#database-operations> such as @findAndModify@, or the SQL keyword.

[Notes]: When setting this to an SQL keyword, it is not recommended to attempt any client-side parsing of @db.statement@ just to get this property, but it should be set if the operation name is provided by the library being instrumented. If the SQL statement has an ambiguous operation, or performs more than one operation, this value may be omitted.
-}
pattern DB_OPERATION :: Key Text
pattern DB_OPERATION <- Key "db.operation" where
  DB_OPERATION = Key "db.operation"

{-|
The Microsoft SQL Server <https://docs.microsoft.com/en-us/sql/connect/jdbc/building-the-connection-url?view=sql-server-ver15> connecting to. This name is used to determine the port of a named instance.

[Notes]: If setting a @db.mssql.instance_name@, @net.peer.port@ is no longer required (but still recommended if non-standard).
-}
pattern DB_MSSQL_INSTANCE_NAME :: Key Text
pattern DB_MSSQL_INSTANCE_NAME <- Key "db.mssql.instance_name" where
  DB_MSSQL_INSTANCE_NAME = Key "db.mssql.instance_name"

{-|
The fetch size used for paging, i.e. how many rows will be returned at once.
-}
pattern DB_CASSANDRA_PAGE_SIZE :: Key Int64
pattern DB_CASSANDRA_PAGE_SIZE <- Key "db.cassandra.page_size" where
  DB_CASSANDRA_PAGE_SIZE = Key "db.cassandra.page_size"

{-|
The consistency level of the query. Based on consistency values from <https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html>.
-}
pattern DB_CASSANDRA_CONSISTENCY_LEVEL :: Key Text
pattern DB_CASSANDRA_CONSISTENCY_LEVEL <- Key "db.cassandra.consistency_level" where
  DB_CASSANDRA_CONSISTENCY_LEVEL = Key "db.cassandra.consistency_level"

{-|
The name of the primary table that the operation is acting upon, including the keyspace name (if applicable).

[Notes]: This mirrors the db.sql.table attribute but references cassandra rather than sql. It is not recommended to attempt any client-side parsing of @db.statement@ just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set.
-}
pattern DB_CASSANDRA_TABLE :: Key Text
pattern DB_CASSANDRA_TABLE <- Key "db.cassandra.table" where
  DB_CASSANDRA_TABLE = Key "db.cassandra.table"

{-|
Whether or not the query is idempotent.
-}
pattern DB_CASSANDRA_IDEMPOTENCE :: Key Bool
pattern DB_CASSANDRA_IDEMPOTENCE <- Key "db.cassandra.idempotence" where
  DB_CASSANDRA_IDEMPOTENCE = Key "db.cassandra.idempotence"

{-|
The number of times a query was speculatively executed. Not set or @0@ if the query was not executed speculatively.
-}
pattern DB_CASSANDRA_SPECULATIVE_EXECUTION_COUNT :: Key Int64
pattern DB_CASSANDRA_SPECULATIVE_EXECUTION_COUNT <- Key "db.cassandra.speculative_execution_count" where
  DB_CASSANDRA_SPECULATIVE_EXECUTION_COUNT = Key "db.cassandra.speculative_execution_count"

{-|
The ID of the coordinating node for a query.
-}
pattern DB_CASSANDRA_COORDINATOR_ID :: Key Text
pattern DB_CASSANDRA_COORDINATOR_ID <- Key "db.cassandra.coordinator.id" where
  DB_CASSANDRA_COORDINATOR_ID = Key "db.cassandra.coordinator.id"

{-|
The data center of the coordinating node for a query.
-}
pattern DB_CASSANDRA_COORDINATOR_DC :: Key Text
pattern DB_CASSANDRA_COORDINATOR_DC <- Key "db.cassandra.coordinator.dc" where
  DB_CASSANDRA_COORDINATOR_DC = Key "db.cassandra.coordinator.dc"

{-|
The index of the database being accessed as used in the <https://redis.io/commands/select>, provided as an integer. To be used instead of the generic @db.name@ attribute.
-}
pattern DB_REDIS_DATABASE_INDEX :: Key Int64
pattern DB_REDIS_DATABASE_INDEX <- Key "db.redis.database_index" where
  DB_REDIS_DATABASE_INDEX = Key "db.redis.database_index"

{-|
The collection being accessed within the database stated in @db.name@.
-}
pattern DB_MONGODB_COLLECTION :: Key Text
pattern DB_MONGODB_COLLECTION <- Key "db.mongodb.collection" where
  DB_MONGODB_COLLECTION = Key "db.mongodb.collection"

{-|
The name of the primary table that the operation is acting upon, including the database name (if applicable).

[Notes]: It is not recommended to attempt any client-side parsing of @db.statement@ just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set.
-}
pattern DB_SQL_TABLE :: Key Text
pattern DB_SQL_TABLE <- Key "db.sql.table" where
  DB_SQL_TABLE = Key "db.sql.table"

{-|
The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it.
-}
pattern EXCEPTION_TYPE :: Key Text
pattern EXCEPTION_TYPE <- Key "exception.type" where
  EXCEPTION_TYPE = Key "exception.type"

{-|
The exception message.
-}
pattern EXCEPTION_MESSAGE :: Key Text
pattern EXCEPTION_MESSAGE <- Key "exception.message" where
  EXCEPTION_MESSAGE = Key "exception.message"

{-|
A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG.
-}
pattern EXCEPTION_STACKTRACE :: Key Text
pattern EXCEPTION_STACKTRACE <- Key "exception.stacktrace" where
  EXCEPTION_STACKTRACE = Key "exception.stacktrace"

{-|
SHOULD be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span.

[Notes]: An exception is considered to have escaped (or left) the scope of a span,
if that span is ended while the exception is still logically &quot;in flight&quot;.
This may be actually &quot;in flight&quot; in some languages (e.g. if the exception
is passed to a Context manager's @__exit__@ method in Python) but will
usually be caught at the point of recording the exception in most languages.It is usually not possible to determine at the point where an exception is thrown
whether it will escape the scope of a span.
However, it is trivial to know that an exception
will escape, if one checks for an active exception just before ending the span,
as done in the <#recording-an-exception>.It follows that an exception may still escape the scope of the span
even if the @exception.escaped@ attribute was not set or set to false,
since the event might have been recorded at a time where it was not
clear whether the exception will escape.
-}
pattern EXCEPTION_ESCAPED :: Key Bool
pattern EXCEPTION_ESCAPED <- Key "exception.escaped" where
  EXCEPTION_ESCAPED = Key "exception.escaped"

{-|
Type of the trigger which caused this function execution.

[Notes]: For the server/consumer span on the incoming side,
@faas.trigger@ MUST be set.Clients invoking FaaS instances usually cannot set @faas.trigger@,
since they would typically need to look in the payload to determine
the event type. If clients set it, it should be the same as the
trigger that corresponding incoming would have (i.e., this has
nothing to do with the underlying transport used to make the API
call to invoke the lambda, which is often HTTP).
-}
pattern FAAS_TRIGGER :: Key Text
pattern FAAS_TRIGGER <- Key "faas.trigger" where
  FAAS_TRIGGER = Key "faas.trigger"

{-|
The execution ID of the current function execution.
-}
pattern FAAS_EXECUTION :: Key Text
pattern FAAS_EXECUTION <- Key "faas.execution" where
  FAAS_EXECUTION = Key "faas.execution"

{-|
The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name.
-}
pattern FAAS_DOCUMENT_COLLECTION :: Key Text
pattern FAAS_DOCUMENT_COLLECTION <- Key "faas.document.collection" where
  FAAS_DOCUMENT_COLLECTION = Key "faas.document.collection"

{-|
Describes the type of the operation that was performed on the data.
-}
pattern FAAS_DOCUMENT_OPERATION :: Key Text
pattern FAAS_DOCUMENT_OPERATION <- Key "faas.document.operation" where
  FAAS_DOCUMENT_OPERATION = Key "faas.document.operation"

{-|
A string containing the time when the data was accessed in the <https://www.iso.org/iso-8601-date-and-time-format.html> format expressed in <https://www.w3.org/TR/NOTE-datetime>.
-}
pattern FAAS_DOCUMENT_TIME :: Key Text
pattern FAAS_DOCUMENT_TIME <- Key "faas.document.time" where
  FAAS_DOCUMENT_TIME = Key "faas.document.time"

{-|
The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name.
-}
pattern FAAS_DOCUMENT_NAME :: Key Text
pattern FAAS_DOCUMENT_NAME <- Key "faas.document.name" where
  FAAS_DOCUMENT_NAME = Key "faas.document.name"

{-|
A string containing the function invocation time in the <https://www.iso.org/iso-8601-date-and-time-format.html> format expressed in <https://www.w3.org/TR/NOTE-datetime>.
-}
pattern FAAS_TIME :: Key Text
pattern FAAS_TIME <- Key "faas.time" where
  FAAS_TIME = Key "faas.time"

{-|
A string containing the schedule period as <https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm>.
-}
pattern FAAS_CRON :: Key Text
pattern FAAS_CRON <- Key "faas.cron" where
  FAAS_CRON = Key "faas.cron"

{-|
A boolean that is true if the serverless function is executed for the first time (aka cold-start).
-}
pattern FAAS_COLDSTART :: Key Bool
pattern FAAS_COLDSTART <- Key "faas.coldstart" where
  FAAS_COLDSTART = Key "faas.coldstart"

{-|
The name of the invoked function.

[Notes]: SHOULD be equal to the @faas.name@ resource attribute of the invoked function.
-}
pattern FAAS_INVOKED_NAME :: Key Text
pattern FAAS_INVOKED_NAME <- Key "faas.invoked_name" where
  FAAS_INVOKED_NAME = Key "faas.invoked_name"

{-|
The cloud provider of the invoked function.

[Notes]: SHOULD be equal to the @cloud.provider@ resource attribute of the invoked function.
-}
pattern FAAS_INVOKED_PROVIDER :: Key Text
pattern FAAS_INVOKED_PROVIDER <- Key "faas.invoked_provider" where
  FAAS_INVOKED_PROVIDER = Key "faas.invoked_provider"

{-|
The cloud region of the invoked function.

[Notes]: SHOULD be equal to the @cloud.region@ resource attribute of the invoked function.
-}
pattern FAAS_INVOKED_REGION :: Key Text
pattern FAAS_INVOKED_REGION <- Key "faas.invoked_region" where
  FAAS_INVOKED_REGION = Key "faas.invoked_region"

{-|
Transport protocol used. See note below.
-}
pattern NET_TRANSPORT :: Key Text
pattern NET_TRANSPORT <- Key "net.transport" where
  NET_TRANSPORT = Key "net.transport"

{-|
Remote address of the peer (dotted decimal for IPv4 or <https://tools.ietf.org/html/rfc5952> for IPv6)
-}
pattern NET_PEER_IP :: Key Text
pattern NET_PEER_IP <- Key "net.peer.ip" where
  NET_PEER_IP = Key "net.peer.ip"

{-|
Remote port number.
-}
pattern NET_PEER_PORT :: Key Int64
pattern NET_PEER_PORT <- Key "net.peer.port" where
  NET_PEER_PORT = Key "net.peer.port"

{-|
Remote hostname or similar, see note below.

[Notes]: @net.peer.name@ SHOULD NOT be set if capturing it would require an extra DNS lookup.
-}
pattern NET_PEER_NAME :: Key Text
pattern NET_PEER_NAME <- Key "net.peer.name" where
  NET_PEER_NAME = Key "net.peer.name"

{-|
Like @net.peer.ip@ but for the host IP. Useful in case of a multi-IP host.
-}
pattern NET_HOST_IP :: Key Text
pattern NET_HOST_IP <- Key "net.host.ip" where
  NET_HOST_IP = Key "net.host.ip"

{-|
Like @net.peer.port@ but for the host port.
-}
pattern NET_HOST_PORT :: Key Int64
pattern NET_HOST_PORT <- Key "net.host.port" where
  NET_HOST_PORT = Key "net.host.port"

{-|
Local hostname or similar, see note below.
-}
pattern NET_HOST_NAME :: Key Text
pattern NET_HOST_NAME <- Key "net.host.name" where
  NET_HOST_NAME = Key "net.host.name"

{-|
The internet connection type currently being used by the host.
-}
pattern NET_HOST_CONNECTION_TYPE :: Key Text
pattern NET_HOST_CONNECTION_TYPE <- Key "net.host.connection.type" where
  NET_HOST_CONNECTION_TYPE = Key "net.host.connection.type"

{-|
This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection.
-}
pattern NET_HOST_CONNECTION_SUBTYPE :: Key Text
pattern NET_HOST_CONNECTION_SUBTYPE <- Key "net.host.connection.subtype" where
  NET_HOST_CONNECTION_SUBTYPE = Key "net.host.connection.subtype"

{-|
The name of the mobile carrier.
-}
pattern NET_HOST_CARRIER_NAME :: Key Text
pattern NET_HOST_CARRIER_NAME <- Key "net.host.carrier.name" where
  NET_HOST_CARRIER_NAME = Key "net.host.carrier.name"

{-|
The mobile carrier country code.
-}
pattern NET_HOST_CARRIER_MCC :: Key Text
pattern NET_HOST_CARRIER_MCC <- Key "net.host.carrier.mcc" where
  NET_HOST_CARRIER_MCC = Key "net.host.carrier.mcc"

{-|
The mobile carrier network code.
-}
pattern NET_HOST_CARRIER_MNC :: Key Text
pattern NET_HOST_CARRIER_MNC <- Key "net.host.carrier.mnc" where
  NET_HOST_CARRIER_MNC = Key "net.host.carrier.mnc"

{-|
The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network.
-}
pattern NET_HOST_CARRIER_ICC :: Key Text
pattern NET_HOST_CARRIER_ICC <- Key "net.host.carrier.icc" where
  NET_HOST_CARRIER_ICC = Key "net.host.carrier.icc"

{-|
The <../../resource/semantic_conventions/README.md#service> of the remote service. SHOULD be equal to the actual @service.name@ resource attribute of the remote service if any.
-}
pattern PEER_SERVICE :: Key Text
pattern PEER_SERVICE <- Key "peer.service" where
  PEER_SERVICE = Key "peer.service"

{-|
Username or client_id extracted from the access token or <https://tools.ietf.org/html/rfc7235#section-4.2> header in the inbound request from outside the system.
-}
pattern ENDUSER_ID :: Key Text
pattern ENDUSER_ID <- Key "enduser.id" where
  ENDUSER_ID = Key "enduser.id"

{-|
Actual/assumed role the client is making the request under extracted from token or application security context.
-}
pattern ENDUSER_ROLE :: Key Text
pattern ENDUSER_ROLE <- Key "enduser.role" where
  ENDUSER_ROLE = Key "enduser.role"

{-|
Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an <https://tools.ietf.org/html/rfc6749#section-3.3> or an attribute value in a <http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html>.
-}
pattern ENDUSER_SCOPE :: Key Text
pattern ENDUSER_SCOPE <- Key "enduser.scope" where
  ENDUSER_SCOPE = Key "enduser.scope"

{-|
Current &quot;managed&quot; thread ID (as opposed to OS thread ID).
-}
pattern THREAD_ID :: Key Int64
pattern THREAD_ID <- Key "thread.id" where
  THREAD_ID = Key "thread.id"

{-|
Current thread name.
-}
pattern THREAD_NAME :: Key Text
pattern THREAD_NAME <- Key "thread.name" where
  THREAD_NAME = Key "thread.name"

{-|
The method or function name, or equivalent (usually rightmost part of the code unit's name).
-}
pattern CODE_FUNCTION :: Key Text
pattern CODE_FUNCTION <- Key "code.function" where
  CODE_FUNCTION = Key "code.function"

{-|
The &quot;namespace&quot; within which @code.function@ is defined. Usually the qualified class or module name, such that @code.namespace@ + some separator + @code.function@ form a unique identifier for the code unit.
-}
pattern CODE_NAMESPACE :: Key Text
pattern CODE_NAMESPACE <- Key "code.namespace" where
  CODE_NAMESPACE = Key "code.namespace"

{-|
The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path).
-}
pattern CODE_FILEPATH :: Key Text
pattern CODE_FILEPATH <- Key "code.filepath" where
  CODE_FILEPATH = Key "code.filepath"

{-|
The line number in @code.filepath@ best representing the operation. It SHOULD point within the code unit named in @code.function@.
-}
pattern CODE_LINENO :: Key Int64
pattern CODE_LINENO <- Key "code.lineno" where
  CODE_LINENO = Key "code.lineno"

{-|
HTTP request method.
-}
pattern HTTP_METHOD :: Key Text
pattern HTTP_METHOD <- Key "http.method" where
  HTTP_METHOD = Key "http.method"

{-|
Full HTTP request URL in the form @scheme:\/\/host[:port]\/path?query[#fragment]@. Usually the fragment is not transmitted over HTTP, but if it is known, it should be included nevertheless.

[Notes]: @http.url@ MUST NOT contain credentials passed via URL in form of @https:\/\/username:password@www.example.com\/@. In such case the attribute's value should be @https:\/\/www.example.com\/@.
-}
pattern HTTP_URL :: Key Text
pattern HTTP_URL <- Key "http.url" where
  HTTP_URL = Key "http.url"

{-|
The full request target as passed in a HTTP request line or equivalent.
-}
pattern HTTP_TARGET :: Key Text
pattern HTTP_TARGET <- Key "http.target" where
  HTTP_TARGET = Key "http.target"

{-|
The value of the <https://tools.ietf.org/html/rfc7230#section-5.4>. An empty Host header should also be reported, see note.

[Notes]: When the header is present but empty the attribute SHOULD be set to the empty string. Note that this is a valid situation that is expected in certain cases, according the aforementioned <https://tools.ietf.org/html/rfc7230#section-5.4>. When the header is not set the attribute MUST NOT be set.
-}
pattern HTTP_HOST :: Key Text
pattern HTTP_HOST <- Key "http.host" where
  HTTP_HOST = Key "http.host"

{-|
The URI scheme identifying the used protocol.
-}
pattern HTTP_SCHEME :: Key Text
pattern HTTP_SCHEME <- Key "http.scheme" where
  HTTP_SCHEME = Key "http.scheme"

{-|
<https://tools.ietf.org/html/rfc7231#section-6>.
-}
pattern HTTP_STATUS_CODE :: Key Int64
pattern HTTP_STATUS_CODE <- Key "http.status_code" where
  HTTP_STATUS_CODE = Key "http.status_code"

{-|
Kind of HTTP protocol used.

[Notes]: If @net.transport@ is not specified, it can be assumed to be @IP.TCP@ except if @http.flavor@ is @QUIC@, in which case @IP.UDP@ is assumed.
-}
pattern HTTP_FLAVOR :: Key Text
pattern HTTP_FLAVOR <- Key "http.flavor" where
  HTTP_FLAVOR = Key "http.flavor"

{-|
Value of the <https://tools.ietf.org/html/rfc7231#section-5.5.3> header sent by the client.
-}
pattern HTTP_USER_AGENT :: Key Text
pattern HTTP_USER_AGENT <- Key "http.user_agent" where
  HTTP_USER_AGENT = Key "http.user_agent"

{-|
The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the <https://tools.ietf.org/html/rfc7230#section-3.3.2> header. For requests using transport encoding, this should be the compressed size.
-}
pattern HTTP_REQUEST_CONTENT_LENGTH :: Key Int64
pattern HTTP_REQUEST_CONTENT_LENGTH <- Key "http.request_content_length" where
  HTTP_REQUEST_CONTENT_LENGTH = Key "http.request_content_length"

{-|
The size of the uncompressed request payload body after transport decoding. Not set if transport encoding not used.
-}
pattern HTTP_REQUEST_CONTENT_LENGTH_UNCOMPRESSED :: Key Int64
pattern HTTP_REQUEST_CONTENT_LENGTH_UNCOMPRESSED <- Key "http.request_content_length_uncompressed" where
  HTTP_REQUEST_CONTENT_LENGTH_UNCOMPRESSED = Key "http.request_content_length_uncompressed"

{-|
The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the <https://tools.ietf.org/html/rfc7230#section-3.3.2> header. For requests using transport encoding, this should be the compressed size.
-}
pattern HTTP_RESPONSE_CONTENT_LENGTH :: Key Int64
pattern HTTP_RESPONSE_CONTENT_LENGTH <- Key "http.response_content_length" where
  HTTP_RESPONSE_CONTENT_LENGTH = Key "http.response_content_length"

{-|
The size of the uncompressed response payload body after transport decoding. Not set if transport encoding not used.
-}
pattern HTTP_RESPONSE_CONTENT_LENGTH_UNCOMPRESSED :: Key Int64
pattern HTTP_RESPONSE_CONTENT_LENGTH_UNCOMPRESSED <- Key "http.response_content_length_uncompressed" where
  HTTP_RESPONSE_CONTENT_LENGTH_UNCOMPRESSED = Key "http.response_content_length_uncompressed"

{-|
The ordinal number of request re-sending attempt.
-}
pattern HTTP_RETRY_COUNT :: Key Int64
pattern HTTP_RETRY_COUNT <- Key "http.retry_count" where
  HTTP_RETRY_COUNT = Key "http.retry_count"

{-|
The primary server name of the matched virtual host. This should be obtained via configuration. If no such configuration can be obtained, this attribute MUST NOT be set ( @net.host.name@ should be used instead).

[Notes]: @http.url@ is usually not readily available on the server side but would have to be assembled in a cumbersome and sometimes lossy process from other information (see e.g. open-telemetry/opentelemetry-python/pull/148). It is thus preferred to supply the raw data that is available.
-}
pattern HTTP_SERVER_NAME :: Key Text
pattern HTTP_SERVER_NAME <- Key "http.server_name" where
  HTTP_SERVER_NAME = Key "http.server_name"

{-|
The matched route (path template).
-}
pattern HTTP_ROUTE :: Key Text
pattern HTTP_ROUTE <- Key "http.route" where
  HTTP_ROUTE = Key "http.route"

{-|
The IP address of the original client behind all proxies, if known (e.g. from <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-For>).

[Notes]: This is not necessarily the same as @net.peer.ip@, which would
identify the network-level peer, which may be a proxy.This attribute should be set when a source of information different
from the one used for @net.peer.ip@, is available even if that other
source just confirms the same value as @net.peer.ip@.
Rationale: For @net.peer.ip@, one typically does not know if it
comes from a proxy, reverse proxy, or the actual client. Setting
@http.client_ip@ when it's the same as @net.peer.ip@ means that
one is at least somewhat confident that the address is not that of
the closest proxy.
-}
pattern HTTP_CLIENT_IP :: Key Text
pattern HTTP_CLIENT_IP <- Key "http.client_ip" where
  HTTP_CLIENT_IP = Key "http.client_ip"

{-|
The keys in the @RequestItems@ object field.
-}
pattern AWS_DYNAMODB_TABLE_NAMES :: Key (AttrVals Text)
pattern AWS_DYNAMODB_TABLE_NAMES <- Key "aws.dynamodb.table_names" where
  AWS_DYNAMODB_TABLE_NAMES = Key "aws.dynamodb.table_names"

{-|
The JSON-serialized value of each item in the @ConsumedCapacity@ response field.
-}
pattern AWS_DYNAMODB_CONSUMED_CAPACITY :: Key (AttrVals Text)
pattern AWS_DYNAMODB_CONSUMED_CAPACITY <- Key "aws.dynamodb.consumed_capacity" where
  AWS_DYNAMODB_CONSUMED_CAPACITY = Key "aws.dynamodb.consumed_capacity"

{-|
The JSON-serialized value of the @ItemCollectionMetrics@ response field.
-}
pattern AWS_DYNAMODB_ITEM_COLLECTION_METRICS :: Key Text
pattern AWS_DYNAMODB_ITEM_COLLECTION_METRICS <- Key "aws.dynamodb.item_collection_metrics" where
  AWS_DYNAMODB_ITEM_COLLECTION_METRICS = Key "aws.dynamodb.item_collection_metrics"

{-|
The value of the @ProvisionedThroughput.ReadCapacityUnits@ request parameter.
-}
pattern AWS_DYNAMODB_PROVISIONED_READ_CAPACITY :: Key Double
pattern AWS_DYNAMODB_PROVISIONED_READ_CAPACITY <- Key "aws.dynamodb.provisioned_read_capacity" where
  AWS_DYNAMODB_PROVISIONED_READ_CAPACITY = Key "aws.dynamodb.provisioned_read_capacity"

{-|
The value of the @ProvisionedThroughput.WriteCapacityUnits@ request parameter.
-}
pattern AWS_DYNAMODB_PROVISIONED_WRITE_CAPACITY :: Key Double
pattern AWS_DYNAMODB_PROVISIONED_WRITE_CAPACITY <- Key "aws.dynamodb.provisioned_write_capacity" where
  AWS_DYNAMODB_PROVISIONED_WRITE_CAPACITY = Key "aws.dynamodb.provisioned_write_capacity"

{-|
The value of the @ConsistentRead@ request parameter.
-}
pattern AWS_DYNAMODB_CONSISTENT_READ :: Key Bool
pattern AWS_DYNAMODB_CONSISTENT_READ <- Key "aws.dynamodb.consistent_read" where
  AWS_DYNAMODB_CONSISTENT_READ = Key "aws.dynamodb.consistent_read"

{-|
The value of the @ProjectionExpression@ request parameter.
-}
pattern AWS_DYNAMODB_PROJECTION :: Key Text
pattern AWS_DYNAMODB_PROJECTION <- Key "aws.dynamodb.projection" where
  AWS_DYNAMODB_PROJECTION = Key "aws.dynamodb.projection"

{-|
The value of the @Limit@ request parameter.
-}
pattern AWS_DYNAMODB_LIMIT :: Key Int64
pattern AWS_DYNAMODB_LIMIT <- Key "aws.dynamodb.limit" where
  AWS_DYNAMODB_LIMIT = Key "aws.dynamodb.limit"

{-|
The value of the @AttributesToGet@ request parameter.
-}
pattern AWS_DYNAMODB_ATTRIBUTES_TO_GET :: Key (AttrVals Text)
pattern AWS_DYNAMODB_ATTRIBUTES_TO_GET <- Key "aws.dynamodb.attributes_to_get" where
  AWS_DYNAMODB_ATTRIBUTES_TO_GET = Key "aws.dynamodb.attributes_to_get"

{-|
The value of the @IndexName@ request parameter.
-}
pattern AWS_DYNAMODB_INDEX_NAME :: Key Text
pattern AWS_DYNAMODB_INDEX_NAME <- Key "aws.dynamodb.index_name" where
  AWS_DYNAMODB_INDEX_NAME = Key "aws.dynamodb.index_name"

{-|
The value of the @Select@ request parameter.
-}
pattern AWS_DYNAMODB_SELECT :: Key Text
pattern AWS_DYNAMODB_SELECT <- Key "aws.dynamodb.select" where
  AWS_DYNAMODB_SELECT = Key "aws.dynamodb.select"

{-|
The JSON-serialized value of each item of the @GlobalSecondaryIndexes@ request field
-}
pattern AWS_DYNAMODB_GLOBAL_SECONDARY_INDEXES :: Key (AttrVals Text)
pattern AWS_DYNAMODB_GLOBAL_SECONDARY_INDEXES <- Key "aws.dynamodb.global_secondary_indexes" where
  AWS_DYNAMODB_GLOBAL_SECONDARY_INDEXES = Key "aws.dynamodb.global_secondary_indexes"

{-|
The JSON-serialized value of each item of the @LocalSecondaryIndexes@ request field.
-}
pattern AWS_DYNAMODB_LOCAL_SECONDARY_INDEXES :: Key (AttrVals Text)
pattern AWS_DYNAMODB_LOCAL_SECONDARY_INDEXES <- Key "aws.dynamodb.local_secondary_indexes" where
  AWS_DYNAMODB_LOCAL_SECONDARY_INDEXES = Key "aws.dynamodb.local_secondary_indexes"

{-|
The value of the @ExclusiveStartTableName@ request parameter.
-}
pattern AWS_DYNAMODB_EXCLUSIVE_START_TABLE :: Key Text
pattern AWS_DYNAMODB_EXCLUSIVE_START_TABLE <- Key "aws.dynamodb.exclusive_start_table" where
  AWS_DYNAMODB_EXCLUSIVE_START_TABLE = Key "aws.dynamodb.exclusive_start_table"

{-|
The the number of items in the @TableNames@ response parameter.
-}
pattern AWS_DYNAMODB_TABLE_COUNT :: Key Int64
pattern AWS_DYNAMODB_TABLE_COUNT <- Key "aws.dynamodb.table_count" where
  AWS_DYNAMODB_TABLE_COUNT = Key "aws.dynamodb.table_count"

{-|
The value of the @ScanIndexForward@ request parameter.
-}
pattern AWS_DYNAMODB_SCAN_FORWARD :: Key Bool
pattern AWS_DYNAMODB_SCAN_FORWARD <- Key "aws.dynamodb.scan_forward" where
  AWS_DYNAMODB_SCAN_FORWARD = Key "aws.dynamodb.scan_forward"

{-|
The value of the @Segment@ request parameter.
-}
pattern AWS_DYNAMODB_SEGMENT :: Key Int64
pattern AWS_DYNAMODB_SEGMENT <- Key "aws.dynamodb.segment" where
  AWS_DYNAMODB_SEGMENT = Key "aws.dynamodb.segment"

{-|
The value of the @TotalSegments@ request parameter.
-}
pattern AWS_DYNAMODB_TOTAL_SEGMENTS :: Key Int64
pattern AWS_DYNAMODB_TOTAL_SEGMENTS <- Key "aws.dynamodb.total_segments" where
  AWS_DYNAMODB_TOTAL_SEGMENTS = Key "aws.dynamodb.total_segments"

{-|
The value of the @Count@ response parameter.
-}
pattern AWS_DYNAMODB_COUNT :: Key Int64
pattern AWS_DYNAMODB_COUNT <- Key "aws.dynamodb.count" where
  AWS_DYNAMODB_COUNT = Key "aws.dynamodb.count"

{-|
The value of the @ScannedCount@ response parameter.
-}
pattern AWS_DYNAMODB_SCANNED_COUNT :: Key Int64
pattern AWS_DYNAMODB_SCANNED_COUNT <- Key "aws.dynamodb.scanned_count" where
  AWS_DYNAMODB_SCANNED_COUNT = Key "aws.dynamodb.scanned_count"

{-|
The JSON-serialized value of each item in the @AttributeDefinitions@ request field.
-}
pattern AWS_DYNAMODB_ATTRIBUTE_DEFINITIONS :: Key (AttrVals Text)
pattern AWS_DYNAMODB_ATTRIBUTE_DEFINITIONS <- Key "aws.dynamodb.attribute_definitions" where
  AWS_DYNAMODB_ATTRIBUTE_DEFINITIONS = Key "aws.dynamodb.attribute_definitions"

{-|
The JSON-serialized value of each item in the the @GlobalSecondaryIndexUpdates@ request field.
-}
pattern AWS_DYNAMODB_GLOBAL_SECONDARY_INDEX_UPDATES :: Key (AttrVals Text)
pattern AWS_DYNAMODB_GLOBAL_SECONDARY_INDEX_UPDATES <- Key "aws.dynamodb.global_secondary_index_updates" where
  AWS_DYNAMODB_GLOBAL_SECONDARY_INDEX_UPDATES = Key "aws.dynamodb.global_secondary_index_updates"

{-|
A string identifying the messaging system.
-}
pattern MESSAGING_SYSTEM :: Key Text
pattern MESSAGING_SYSTEM <- Key "messaging.system" where
  MESSAGING_SYSTEM = Key "messaging.system"

{-|
The message destination name. This might be equal to the span name but is required nevertheless.
-}
pattern MESSAGING_DESTINATION :: Key Text
pattern MESSAGING_DESTINATION <- Key "messaging.destination" where
  MESSAGING_DESTINATION = Key "messaging.destination"

{-|
The kind of message destination
-}
pattern MESSAGING_DESTINATION_KIND :: Key Text
pattern MESSAGING_DESTINATION_KIND <- Key "messaging.destination_kind" where
  MESSAGING_DESTINATION_KIND = Key "messaging.destination_kind"

{-|
A boolean that is true if the message destination is temporary.
-}
pattern MESSAGING_TEMP_DESTINATION :: Key Bool
pattern MESSAGING_TEMP_DESTINATION <- Key "messaging.temp_destination" where
  MESSAGING_TEMP_DESTINATION = Key "messaging.temp_destination"

{-|
The name of the transport protocol.
-}
pattern MESSAGING_PROTOCOL :: Key Text
pattern MESSAGING_PROTOCOL <- Key "messaging.protocol" where
  MESSAGING_PROTOCOL = Key "messaging.protocol"

{-|
The version of the transport protocol.
-}
pattern MESSAGING_PROTOCOL_VERSION :: Key Text
pattern MESSAGING_PROTOCOL_VERSION <- Key "messaging.protocol_version" where
  MESSAGING_PROTOCOL_VERSION = Key "messaging.protocol_version"

{-|
Connection string.
-}
pattern MESSAGING_URL :: Key Text
pattern MESSAGING_URL <- Key "messaging.url" where
  MESSAGING_URL = Key "messaging.url"

{-|
A value used by the messaging system as an identifier for the message, represented as a string.
-}
pattern MESSAGING_MESSAGE_ID :: Key Text
pattern MESSAGING_MESSAGE_ID <- Key "messaging.message_id" where
  MESSAGING_MESSAGE_ID = Key "messaging.message_id"

{-|
The <#conversations> identifying the conversation to which the message belongs, represented as a string. Sometimes called &quot;Correlation ID&quot;.
-}
pattern MESSAGING_CONVERSATION_ID :: Key Text
pattern MESSAGING_CONVERSATION_ID <- Key "messaging.conversation_id" where
  MESSAGING_CONVERSATION_ID = Key "messaging.conversation_id"

{-|
The (uncompressed) size of the message payload in bytes. Also use this attribute if it is unknown whether the compressed or uncompressed payload size is reported.
-}
pattern MESSAGING_MESSAGE_PAYLOAD_SIZE_BYTES :: Key Int64
pattern MESSAGING_MESSAGE_PAYLOAD_SIZE_BYTES <- Key "messaging.message_payload_size_bytes" where
  MESSAGING_MESSAGE_PAYLOAD_SIZE_BYTES = Key "messaging.message_payload_size_bytes"

{-|
The compressed size of the message payload in bytes.
-}
pattern MESSAGING_MESSAGE_PAYLOAD_COMPRESSED_SIZE_BYTES :: Key Int64
pattern MESSAGING_MESSAGE_PAYLOAD_COMPRESSED_SIZE_BYTES <- Key "messaging.message_payload_compressed_size_bytes" where
  MESSAGING_MESSAGE_PAYLOAD_COMPRESSED_SIZE_BYTES = Key "messaging.message_payload_compressed_size_bytes"

{-|
A string identifying the kind of message consumption as defined in the <#operation-names> section above. If the operation is &quot;send&quot;, this attribute MUST NOT be set, since the operation can be inferred from the span kind in that case.
-}
pattern MESSAGING_OPERATION :: Key Text
pattern MESSAGING_OPERATION <- Key "messaging.operation" where
  MESSAGING_OPERATION = Key "messaging.operation"

{-|
The identifier for the consumer receiving a message. For Kafka, set it to @{messaging.kafka.consumer_group} - {messaging.kafka.client_id}@, if both are present, or only @messaging.kafka.consumer_group@. For brokers, such as RabbitMQ and Artemis, set it to the @client_id@ of the client consuming the message.
-}
pattern MESSAGING_CONSUMER_ID :: Key Text
pattern MESSAGING_CONSUMER_ID <- Key "messaging.consumer_id" where
  MESSAGING_CONSUMER_ID = Key "messaging.consumer_id"

{-|
RabbitMQ message routing key.
-}
pattern MESSAGING_RABBITMQ_ROUTING_KEY :: Key Text
pattern MESSAGING_RABBITMQ_ROUTING_KEY <- Key "messaging.rabbitmq.routing_key" where
  MESSAGING_RABBITMQ_ROUTING_KEY = Key "messaging.rabbitmq.routing_key"

{-|
Message keys in Kafka are used for grouping alike messages to ensure they're processed on the same partition. They differ from @messaging.message_id@ in that they're not unique. If the key is @null@, the attribute MUST NOT be set.

[Notes]: If the key type is not string, it's string representation has to be supplied for the attribute. If the key has no unambiguous, canonical string form, don't include its value.
-}
pattern MESSAGING_KAFKA_MESSAGE_KEY :: Key Text
pattern MESSAGING_KAFKA_MESSAGE_KEY <- Key "messaging.kafka.message_key" where
  MESSAGING_KAFKA_MESSAGE_KEY = Key "messaging.kafka.message_key"

{-|
Name of the Kafka Consumer Group that is handling the message. Only applies to consumers, not producers.
-}
pattern MESSAGING_KAFKA_CONSUMER_GROUP :: Key Text
pattern MESSAGING_KAFKA_CONSUMER_GROUP <- Key "messaging.kafka.consumer_group" where
  MESSAGING_KAFKA_CONSUMER_GROUP = Key "messaging.kafka.consumer_group"

{-|
Client Id for the Consumer or Producer that is handling the message.
-}
pattern MESSAGING_KAFKA_CLIENT_ID :: Key Text
pattern MESSAGING_KAFKA_CLIENT_ID <- Key "messaging.kafka.client_id" where
  MESSAGING_KAFKA_CLIENT_ID = Key "messaging.kafka.client_id"

{-|
Partition the message is sent to.
-}
pattern MESSAGING_KAFKA_PARTITION :: Key Int64
pattern MESSAGING_KAFKA_PARTITION <- Key "messaging.kafka.partition" where
  MESSAGING_KAFKA_PARTITION = Key "messaging.kafka.partition"

{-|
A boolean that is true if the message is a tombstone.
-}
pattern MESSAGING_KAFKA_TOMBSTONE :: Key Bool
pattern MESSAGING_KAFKA_TOMBSTONE <- Key "messaging.kafka.tombstone" where
  MESSAGING_KAFKA_TOMBSTONE = Key "messaging.kafka.tombstone"

{-|
Namespace of RocketMQ resources, resources in different namespaces are individual.
-}
pattern MESSAGING_ROCKETMQ_NAMESPACE :: Key Text
pattern MESSAGING_ROCKETMQ_NAMESPACE <- Key "messaging.rocketmq.namespace" where
  MESSAGING_ROCKETMQ_NAMESPACE = Key "messaging.rocketmq.namespace"

{-|
Name of the RocketMQ producer/consumer group that is handling the message. The client type is identified by the SpanKind.
-}
pattern MESSAGING_ROCKETMQ_CLIENT_GROUP :: Key Text
pattern MESSAGING_ROCKETMQ_CLIENT_GROUP <- Key "messaging.rocketmq.client_group" where
  MESSAGING_ROCKETMQ_CLIENT_GROUP = Key "messaging.rocketmq.client_group"

{-|
The unique identifier for each client.
-}
pattern MESSAGING_ROCKETMQ_CLIENT_ID :: Key Text
pattern MESSAGING_ROCKETMQ_CLIENT_ID <- Key "messaging.rocketmq.client_id" where
  MESSAGING_ROCKETMQ_CLIENT_ID = Key "messaging.rocketmq.client_id"

{-|
Type of message.
-}
pattern MESSAGING_ROCKETMQ_MESSAGE_TYPE :: Key Text
pattern MESSAGING_ROCKETMQ_MESSAGE_TYPE <- Key "messaging.rocketmq.message_type" where
  MESSAGING_ROCKETMQ_MESSAGE_TYPE = Key "messaging.rocketmq.message_type"

{-|
The secondary classifier of message besides topic.
-}
pattern MESSAGING_ROCKETMQ_MESSAGE_TAG :: Key Text
pattern MESSAGING_ROCKETMQ_MESSAGE_TAG <- Key "messaging.rocketmq.message_tag" where
  MESSAGING_ROCKETMQ_MESSAGE_TAG = Key "messaging.rocketmq.message_tag"

{-|
Key(s) of message, another way to mark message besides message id.
-}
pattern MESSAGING_ROCKETMQ_MESSAGE_KEYS :: Key (AttrVals Text)
pattern MESSAGING_ROCKETMQ_MESSAGE_KEYS <- Key "messaging.rocketmq.message_keys" where
  MESSAGING_ROCKETMQ_MESSAGE_KEYS = Key "messaging.rocketmq.message_keys"

{-|
Model of message consumption. This only applies to consumer spans.
-}
pattern MESSAGING_ROCKETMQ_CONSUMPTION_MODEL :: Key Text
pattern MESSAGING_ROCKETMQ_CONSUMPTION_MODEL <- Key "messaging.rocketmq.consumption_model" where
  MESSAGING_ROCKETMQ_CONSUMPTION_MODEL = Key "messaging.rocketmq.consumption_model"

{-|
A string identifying the remoting system. See below for a list of well-known identifiers.
-}
pattern RPC_SYSTEM :: Key Text
pattern RPC_SYSTEM <- Key "rpc.system" where
  RPC_SYSTEM = Key "rpc.system"

{-|
The full (logical) name of the service being called, including its package name, if applicable.

[Notes]: This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The @code.namespace@ attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side).
-}
pattern RPC_SERVICE :: Key Text
pattern RPC_SERVICE <- Key "rpc.service" where
  RPC_SERVICE = Key "rpc.service"

{-|
The name of the (logical) method being called, must be equal to the $method part in the span name.

[Notes]: This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The @code.function@ attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side).
-}
pattern RPC_METHOD :: Key Text
pattern RPC_METHOD <- Key "rpc.method" where
  RPC_METHOD = Key "rpc.method"

{-|
The <https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md> of the gRPC request.
-}
pattern RPC_GRPC_STATUS_CODE :: Key Int64
pattern RPC_GRPC_STATUS_CODE <- Key "rpc.grpc.status_code" where
  RPC_GRPC_STATUS_CODE = Key "rpc.grpc.status_code"

{-|
Protocol version as in @jsonrpc@ property of request/response. Since JSON-RPC 1.0 does not specify this, the value can be omitted.
-}
pattern RPC_JSONRPC_VERSION :: Key Text
pattern RPC_JSONRPC_VERSION <- Key "rpc.jsonrpc.version" where
  RPC_JSONRPC_VERSION = Key "rpc.jsonrpc.version"

{-|
@id@ property of request or response. Since protocol allows id to be int, string, @null@ or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of @null@ value. Omit entirely if this is a notification.
-}
pattern RPC_JSONRPC_REQUEST_ID :: Key Text
pattern RPC_JSONRPC_REQUEST_ID <- Key "rpc.jsonrpc.request_id" where
  RPC_JSONRPC_REQUEST_ID = Key "rpc.jsonrpc.request_id"

{-|
@error.code@ property of response if it is an error response.
-}
pattern RPC_JSONRPC_ERROR_CODE :: Key Int64
pattern RPC_JSONRPC_ERROR_CODE <- Key "rpc.jsonrpc.error_code" where
  RPC_JSONRPC_ERROR_CODE = Key "rpc.jsonrpc.error_code"

{-|
@error.message@ property of response if it is an error response.
-}
pattern RPC_JSONRPC_ERROR_MESSAGE :: Key Text
pattern RPC_JSONRPC_ERROR_MESSAGE <- Key "rpc.jsonrpc.error_message" where
  RPC_JSONRPC_ERROR_MESSAGE = Key "rpc.jsonrpc.error_message"

{-|
Whether this is a received or sent message.
-}
pattern MESSAGE_TYPE :: Key Text
pattern MESSAGE_TYPE <- Key "message.type" where
  MESSAGE_TYPE = Key "message.type"

{-|
MUST be calculated as two different counters starting from @1@ one for sent messages and one for received message.

[Notes]: This way we guarantee that the values will be consistent between different implementations.
-}
pattern MESSAGE_ID :: Key Int64
pattern MESSAGE_ID <- Key "message.id" where
  MESSAGE_ID = Key "message.id"

{-|
Compressed size of the message in bytes.
-}
pattern MESSAGE_COMPRESSED_SIZE :: Key Int64
pattern MESSAGE_COMPRESSED_SIZE <- Key "message.compressed_size" where
  MESSAGE_COMPRESSED_SIZE = Key "message.compressed_size"

{-|
Uncompressed size of the message in bytes.
-}
pattern MESSAGE_UNCOMPRESSED_SIZE :: Key Int64
pattern MESSAGE_UNCOMPRESSED_SIZE <- Key "message.uncompressed_size" where
  MESSAGE_UNCOMPRESSED_SIZE = Key "message.uncompressed_size"
data OpentracingRefType
  = OpentracingRefTypeChildOf -- ^ The parent Span depends on the child Span in some capacity.
  | OpentracingRefTypeFollowsFrom -- ^ The parent Span does not depend in any way on the result of the child Span.

instance ToAttrVal OpentracingRefType Text where
  toAttrVal = \case
    OpentracingRefTypeChildOf -> "child_of"
    OpentracingRefTypeFollowsFrom -> "follows_from"

data DbSystem
  = DbSystemOtherSql -- ^ Some other SQL database. Fallback only. See notes.
  | DbSystemMssql -- ^ Microsoft SQL Server.
  | DbSystemMysql -- ^ MySQL.
  | DbSystemOracle -- ^ Oracle Database.
  | DbSystemDb2 -- ^ IBM Db2.
  | DbSystemPostgresql -- ^ PostgreSQL.
  | DbSystemRedshift -- ^ Amazon Redshift.
  | DbSystemHive -- ^ Apache Hive.
  | DbSystemCloudscape -- ^ Cloudscape.
  | DbSystemHsqldb -- ^ HyperSQL DataBase.
  | DbSystemProgress -- ^ Progress Database.
  | DbSystemMaxdb -- ^ SAP MaxDB.
  | DbSystemHanadb -- ^ SAP HANA.
  | DbSystemIngres -- ^ Ingres.
  | DbSystemFirstsql -- ^ FirstSQL.
  | DbSystemEdb -- ^ EnterpriseDB.
  | DbSystemCache -- ^ InterSystems Caché.
  | DbSystemAdabas -- ^ Adabas (Adaptable Database System).
  | DbSystemFirebird -- ^ Firebird.
  | DbSystemDerby -- ^ Apache Derby.
  | DbSystemFilemaker -- ^ FileMaker.
  | DbSystemInformix -- ^ Informix.
  | DbSystemInstantdb -- ^ InstantDB.
  | DbSystemInterbase -- ^ InterBase.
  | DbSystemMariadb -- ^ MariaDB.
  | DbSystemNetezza -- ^ Netezza.
  | DbSystemPervasive -- ^ Pervasive PSQL.
  | DbSystemPointbase -- ^ PointBase.
  | DbSystemSqlite -- ^ SQLite.
  | DbSystemSybase -- ^ Sybase.
  | DbSystemTeradata -- ^ Teradata.
  | DbSystemVertica -- ^ Vertica.
  | DbSystemH2 -- ^ H2.
  | DbSystemColdfusion -- ^ ColdFusion IMQ.
  | DbSystemCassandra -- ^ Apache Cassandra.
  | DbSystemHbase -- ^ Apache HBase.
  | DbSystemMongodb -- ^ MongoDB.
  | DbSystemRedis -- ^ Redis.
  | DbSystemCouchbase -- ^ Couchbase.
  | DbSystemCouchdb -- ^ CouchDB.
  | DbSystemCosmosdb -- ^ Microsoft Azure Cosmos DB.
  | DbSystemDynamodb -- ^ Amazon DynamoDB.
  | DbSystemNeo4j -- ^ Neo4j.
  | DbSystemGeode -- ^ Apache Geode.
  | DbSystemElasticsearch -- ^ Elasticsearch.
  | DbSystemMemcached -- ^ Memcached.
  | DbSystemCockroachdb -- ^ CockroachDB.

instance ToAttrVal DbSystem Text where
  toAttrVal = \case
    DbSystemOtherSql -> "other_sql"
    DbSystemMssql -> "mssql"
    DbSystemMysql -> "mysql"
    DbSystemOracle -> "oracle"
    DbSystemDb2 -> "db2"
    DbSystemPostgresql -> "postgresql"
    DbSystemRedshift -> "redshift"
    DbSystemHive -> "hive"
    DbSystemCloudscape -> "cloudscape"
    DbSystemHsqldb -> "hsqldb"
    DbSystemProgress -> "progress"
    DbSystemMaxdb -> "maxdb"
    DbSystemHanadb -> "hanadb"
    DbSystemIngres -> "ingres"
    DbSystemFirstsql -> "firstsql"
    DbSystemEdb -> "edb"
    DbSystemCache -> "cache"
    DbSystemAdabas -> "adabas"
    DbSystemFirebird -> "firebird"
    DbSystemDerby -> "derby"
    DbSystemFilemaker -> "filemaker"
    DbSystemInformix -> "informix"
    DbSystemInstantdb -> "instantdb"
    DbSystemInterbase -> "interbase"
    DbSystemMariadb -> "mariadb"
    DbSystemNetezza -> "netezza"
    DbSystemPervasive -> "pervasive"
    DbSystemPointbase -> "pointbase"
    DbSystemSqlite -> "sqlite"
    DbSystemSybase -> "sybase"
    DbSystemTeradata -> "teradata"
    DbSystemVertica -> "vertica"
    DbSystemH2 -> "h2"
    DbSystemColdfusion -> "coldfusion"
    DbSystemCassandra -> "cassandra"
    DbSystemHbase -> "hbase"
    DbSystemMongodb -> "mongodb"
    DbSystemRedis -> "redis"
    DbSystemCouchbase -> "couchbase"
    DbSystemCouchdb -> "couchdb"
    DbSystemCosmosdb -> "cosmosdb"
    DbSystemDynamodb -> "dynamodb"
    DbSystemNeo4j -> "neo4j"
    DbSystemGeode -> "geode"
    DbSystemElasticsearch -> "elasticsearch"
    DbSystemMemcached -> "memcached"
    DbSystemCockroachdb -> "cockroachdb"

data DbCassandraConsistencyLevel
  = DbCassandraConsistencyLevelAll -- ^ all.
  | DbCassandraConsistencyLevelEachQuorum -- ^ each_quorum.
  | DbCassandraConsistencyLevelQuorum -- ^ quorum.
  | DbCassandraConsistencyLevelLocalQuorum -- ^ local_quorum.
  | DbCassandraConsistencyLevelOne -- ^ one.
  | DbCassandraConsistencyLevelTwo -- ^ two.
  | DbCassandraConsistencyLevelThree -- ^ three.
  | DbCassandraConsistencyLevelLocalOne -- ^ local_one.
  | DbCassandraConsistencyLevelAny -- ^ any.
  | DbCassandraConsistencyLevelSerial -- ^ serial.
  | DbCassandraConsistencyLevelLocalSerial -- ^ local_serial.

instance ToAttrVal DbCassandraConsistencyLevel Text where
  toAttrVal = \case
    DbCassandraConsistencyLevelAll -> "all"
    DbCassandraConsistencyLevelEachQuorum -> "each_quorum"
    DbCassandraConsistencyLevelQuorum -> "quorum"
    DbCassandraConsistencyLevelLocalQuorum -> "local_quorum"
    DbCassandraConsistencyLevelOne -> "one"
    DbCassandraConsistencyLevelTwo -> "two"
    DbCassandraConsistencyLevelThree -> "three"
    DbCassandraConsistencyLevelLocalOne -> "local_one"
    DbCassandraConsistencyLevelAny -> "any"
    DbCassandraConsistencyLevelSerial -> "serial"
    DbCassandraConsistencyLevelLocalSerial -> "local_serial"

data FaasTrigger
  = FaasTriggerDatasource -- ^ A response to some data source operation such as a database or filesystem read/write.
  | FaasTriggerHttp -- ^ To provide an answer to an inbound HTTP request.
  | FaasTriggerPubsub -- ^ A function is set to be executed when messages are sent to a messaging system.
  | FaasTriggerTimer -- ^ A function is scheduled to be executed regularly.
  | FaasTriggerOther -- ^ If none of the others apply.

instance ToAttrVal FaasTrigger Text where
  toAttrVal = \case
    FaasTriggerDatasource -> "datasource"
    FaasTriggerHttp -> "http"
    FaasTriggerPubsub -> "pubsub"
    FaasTriggerTimer -> "timer"
    FaasTriggerOther -> "other"

data FaasDocumentOperation
  = FaasDocumentOperationInsert -- ^ When a new object is created.
  | FaasDocumentOperationEdit -- ^ When an object is modified.
  | FaasDocumentOperationDelete -- ^ When an object is deleted.

instance ToAttrVal FaasDocumentOperation Text where
  toAttrVal = \case
    FaasDocumentOperationInsert -> "insert"
    FaasDocumentOperationEdit -> "edit"
    FaasDocumentOperationDelete -> "delete"

data FaasInvokedProvider
  = FaasInvokedProviderAlibabaCloud -- ^ Alibaba Cloud.
  | FaasInvokedProviderAws -- ^ Amazon Web Services.
  | FaasInvokedProviderAzure -- ^ Microsoft Azure.
  | FaasInvokedProviderGcp -- ^ Google Cloud Platform.
  | FaasInvokedProviderTencentCloud -- ^ Tencent Cloud.

instance ToAttrVal FaasInvokedProvider Text where
  toAttrVal = \case
    FaasInvokedProviderAlibabaCloud -> "alibaba_cloud"
    FaasInvokedProviderAws -> "aws"
    FaasInvokedProviderAzure -> "azure"
    FaasInvokedProviderGcp -> "gcp"
    FaasInvokedProviderTencentCloud -> "tencent_cloud"

data NetTransport
  = NetTransportIpTcp -- ^ ip_tcp.
  | NetTransportIpUdp -- ^ ip_udp.
  | NetTransportIp -- ^ Another IP-based protocol.
  | NetTransportUnix -- ^ Unix Domain socket. See below.
  | NetTransportPipe -- ^ Named or anonymous pipe. See note below.
  | NetTransportInproc -- ^ In-process communication.
  | NetTransportOther -- ^ Something else (non IP-based).

instance ToAttrVal NetTransport Text where
  toAttrVal = \case
    NetTransportIpTcp -> "ip_tcp"
    NetTransportIpUdp -> "ip_udp"
    NetTransportIp -> "ip"
    NetTransportUnix -> "unix"
    NetTransportPipe -> "pipe"
    NetTransportInproc -> "inproc"
    NetTransportOther -> "other"

data NetHostConnectionType
  = NetHostConnectionTypeWifi -- ^ wifi.
  | NetHostConnectionTypeWired -- ^ wired.
  | NetHostConnectionTypeCell -- ^ cell.
  | NetHostConnectionTypeUnavailable -- ^ unavailable.
  | NetHostConnectionTypeUnknown -- ^ unknown.

instance ToAttrVal NetHostConnectionType Text where
  toAttrVal = \case
    NetHostConnectionTypeWifi -> "wifi"
    NetHostConnectionTypeWired -> "wired"
    NetHostConnectionTypeCell -> "cell"
    NetHostConnectionTypeUnavailable -> "unavailable"
    NetHostConnectionTypeUnknown -> "unknown"

data NetHostConnectionSubtype
  = NetHostConnectionSubtypeGprs -- ^ GPRS.
  | NetHostConnectionSubtypeEdge -- ^ EDGE.
  | NetHostConnectionSubtypeUmts -- ^ UMTS.
  | NetHostConnectionSubtypeCdma -- ^ CDMA.
  | NetHostConnectionSubtypeEvdo0 -- ^ EVDO Rel. 0.
  | NetHostConnectionSubtypeEvdoA -- ^ EVDO Rev. A.
  | NetHostConnectionSubtypeCdma20001xrtt -- ^ CDMA2000 1XRTT.
  | NetHostConnectionSubtypeHsdpa -- ^ HSDPA.
  | NetHostConnectionSubtypeHsupa -- ^ HSUPA.
  | NetHostConnectionSubtypeHspa -- ^ HSPA.
  | NetHostConnectionSubtypeIden -- ^ IDEN.
  | NetHostConnectionSubtypeEvdoB -- ^ EVDO Rev. B.
  | NetHostConnectionSubtypeLte -- ^ LTE.
  | NetHostConnectionSubtypeEhrpd -- ^ EHRPD.
  | NetHostConnectionSubtypeHspap -- ^ HSPAP.
  | NetHostConnectionSubtypeGsm -- ^ GSM.
  | NetHostConnectionSubtypeTdScdma -- ^ TD-SCDMA.
  | NetHostConnectionSubtypeIwlan -- ^ IWLAN.
  | NetHostConnectionSubtypeNr -- ^ 5G NR (New Radio).
  | NetHostConnectionSubtypeNrnsa -- ^ 5G NRNSA (New Radio Non-Standalone).
  | NetHostConnectionSubtypeLteCa -- ^ LTE CA.

instance ToAttrVal NetHostConnectionSubtype Text where
  toAttrVal = \case
    NetHostConnectionSubtypeGprs -> "gprs"
    NetHostConnectionSubtypeEdge -> "edge"
    NetHostConnectionSubtypeUmts -> "umts"
    NetHostConnectionSubtypeCdma -> "cdma"
    NetHostConnectionSubtypeEvdo0 -> "evdo_0"
    NetHostConnectionSubtypeEvdoA -> "evdo_a"
    NetHostConnectionSubtypeCdma20001xrtt -> "cdma2000_1xrtt"
    NetHostConnectionSubtypeHsdpa -> "hsdpa"
    NetHostConnectionSubtypeHsupa -> "hsupa"
    NetHostConnectionSubtypeHspa -> "hspa"
    NetHostConnectionSubtypeIden -> "iden"
    NetHostConnectionSubtypeEvdoB -> "evdo_b"
    NetHostConnectionSubtypeLte -> "lte"
    NetHostConnectionSubtypeEhrpd -> "ehrpd"
    NetHostConnectionSubtypeHspap -> "hspap"
    NetHostConnectionSubtypeGsm -> "gsm"
    NetHostConnectionSubtypeTdScdma -> "td_scdma"
    NetHostConnectionSubtypeIwlan -> "iwlan"
    NetHostConnectionSubtypeNr -> "nr"
    NetHostConnectionSubtypeNrnsa -> "nrnsa"
    NetHostConnectionSubtypeLteCa -> "lte_ca"

data HttpFlavor
  = HttpFlavorHttp10 -- ^ HTTP/1.0.
  | HttpFlavorHttp11 -- ^ HTTP/1.1.
  | HttpFlavorHttp20 -- ^ HTTP/2.
  | HttpFlavorHttp30 -- ^ HTTP/3.
  | HttpFlavorSpdy -- ^ SPDY protocol.
  | HttpFlavorQuic -- ^ QUIC protocol.

instance ToAttrVal HttpFlavor Text where
  toAttrVal = \case
    HttpFlavorHttp10 -> "http_1_0"
    HttpFlavorHttp11 -> "http_1_1"
    HttpFlavorHttp20 -> "http_2_0"
    HttpFlavorHttp30 -> "http_3_0"
    HttpFlavorSpdy -> "spdy"
    HttpFlavorQuic -> "quic"

data MessagingDestinationKind
  = MessagingDestinationKindQueue -- ^ A message sent to a queue.
  | MessagingDestinationKindTopic -- ^ A message sent to a topic.

instance ToAttrVal MessagingDestinationKind Text where
  toAttrVal = \case
    MessagingDestinationKindQueue -> "queue"
    MessagingDestinationKindTopic -> "topic"

data MessagingOperation
  = MessagingOperationReceive -- ^ receive.
  | MessagingOperationProcess -- ^ process.

instance ToAttrVal MessagingOperation Text where
  toAttrVal = \case
    MessagingOperationReceive -> "receive"
    MessagingOperationProcess -> "process"

data MessagingRocketmqMessageType
  = MessagingRocketmqMessageTypeNormal -- ^ Normal message.
  | MessagingRocketmqMessageTypeFifo -- ^ FIFO message.
  | MessagingRocketmqMessageTypeDelay -- ^ Delay message.
  | MessagingRocketmqMessageTypeTransaction -- ^ Transaction message.

instance ToAttrVal MessagingRocketmqMessageType Text where
  toAttrVal = \case
    MessagingRocketmqMessageTypeNormal -> "normal"
    MessagingRocketmqMessageTypeFifo -> "fifo"
    MessagingRocketmqMessageTypeDelay -> "delay"
    MessagingRocketmqMessageTypeTransaction -> "transaction"

data MessagingRocketmqConsumptionModel
  = MessagingRocketmqConsumptionModelClustering -- ^ Clustering consumption model.
  | MessagingRocketmqConsumptionModelBroadcasting -- ^ Broadcasting consumption model.

instance ToAttrVal MessagingRocketmqConsumptionModel Text where
  toAttrVal = \case
    MessagingRocketmqConsumptionModelClustering -> "clustering"
    MessagingRocketmqConsumptionModelBroadcasting -> "broadcasting"

data RpcSystem
  = RpcSystemGrpc -- ^ gRPC.
  | RpcSystemJavaRmi -- ^ Java RMI.
  | RpcSystemDotnetWcf -- ^ .NET WCF.
  | RpcSystemApacheDubbo -- ^ Apache Dubbo.

instance ToAttrVal RpcSystem Text where
  toAttrVal = \case
    RpcSystemGrpc -> "grpc"
    RpcSystemJavaRmi -> "java_rmi"
    RpcSystemDotnetWcf -> "dotnet_wcf"
    RpcSystemApacheDubbo -> "apache_dubbo"

data RpcGrpcStatusCode
  = RpcGrpcStatusCodeOk -- ^ OK.
  | RpcGrpcStatusCodeCancelled -- ^ CANCELLED.
  | RpcGrpcStatusCodeUnknown -- ^ UNKNOWN.
  | RpcGrpcStatusCodeInvalidArgument -- ^ INVALID_ARGUMENT.
  | RpcGrpcStatusCodeDeadlineExceeded -- ^ DEADLINE_EXCEEDED.
  | RpcGrpcStatusCodeNotFound -- ^ NOT_FOUND.
  | RpcGrpcStatusCodeAlreadyExists -- ^ ALREADY_EXISTS.
  | RpcGrpcStatusCodePermissionDenied -- ^ PERMISSION_DENIED.
  | RpcGrpcStatusCodeResourceExhausted -- ^ RESOURCE_EXHAUSTED.
  | RpcGrpcStatusCodeFailedPrecondition -- ^ FAILED_PRECONDITION.
  | RpcGrpcStatusCodeAborted -- ^ ABORTED.
  | RpcGrpcStatusCodeOutOfRange -- ^ OUT_OF_RANGE.
  | RpcGrpcStatusCodeUnimplemented -- ^ UNIMPLEMENTED.
  | RpcGrpcStatusCodeInternal -- ^ INTERNAL.
  | RpcGrpcStatusCodeUnavailable -- ^ UNAVAILABLE.
  | RpcGrpcStatusCodeDataLoss -- ^ DATA_LOSS.
  | RpcGrpcStatusCodeUnauthenticated -- ^ UNAUTHENTICATED.

instance ToAttrVal RpcGrpcStatusCode Text where
  toAttrVal = \case
    RpcGrpcStatusCodeOk -> "ok"
    RpcGrpcStatusCodeCancelled -> "cancelled"
    RpcGrpcStatusCodeUnknown -> "unknown"
    RpcGrpcStatusCodeInvalidArgument -> "invalid_argument"
    RpcGrpcStatusCodeDeadlineExceeded -> "deadline_exceeded"
    RpcGrpcStatusCodeNotFound -> "not_found"
    RpcGrpcStatusCodeAlreadyExists -> "already_exists"
    RpcGrpcStatusCodePermissionDenied -> "permission_denied"
    RpcGrpcStatusCodeResourceExhausted -> "resource_exhausted"
    RpcGrpcStatusCodeFailedPrecondition -> "failed_precondition"
    RpcGrpcStatusCodeAborted -> "aborted"
    RpcGrpcStatusCodeOutOfRange -> "out_of_range"
    RpcGrpcStatusCodeUnimplemented -> "unimplemented"
    RpcGrpcStatusCodeInternal -> "internal"
    RpcGrpcStatusCodeUnavailable -> "unavailable"
    RpcGrpcStatusCodeDataLoss -> "data_loss"
    RpcGrpcStatusCodeUnauthenticated -> "unauthenticated"

data MessageType
  = MessageTypeSent -- ^ sent.
  | MessageTypeReceived -- ^ received.

instance ToAttrVal MessageType Text where
  toAttrVal = \case
    MessageTypeSent -> "sent"
    MessageTypeReceived -> "received"

{-|
The name of the keyspace being accessed.

/Deprecated:/ This item has been removed as of 1.8.0 of the semantic conventions. Please use 'DB_NAME' instead.
-}
pattern DB_CASSANDRA_KEYSPACE :: Key Text
pattern DB_CASSANDRA_KEYSPACE <- Key "db.cassandra.keyspace" where
  DB_CASSANDRA_KEYSPACE = Key "db.cassandra.keyspace"
{-# DEPRECATED DB_CASSANDRA_KEYSPACE "This item has been removed as of 1.8.0 of the semantic conventions. Please use DB_NAME instead." #-}

{-|
The [HBase namespace](https://hbase.apache.org/book.html#_namespace) being accessed.

/Deprecated:/ This item has been removed as of 1.8.0 of the semantic conventions. Please use 'DB_NAME' instead.
-}
pattern DB_HBASE_NAMESPACE :: Key Text
pattern DB_HBASE_NAMESPACE <- Key "db.hbase.namespace" where
  DB_HBASE_NAMESPACE = Key "db.hbase.namespace"
{-# DEPRECATED DB_HBASE_NAMESPACE "This item has been removed as of 1.8.0 of the semantic conventions. Please use DB_NAME instead." #-}