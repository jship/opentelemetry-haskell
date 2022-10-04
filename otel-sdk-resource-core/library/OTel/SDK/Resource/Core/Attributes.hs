{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module OTel.SDK.Resource.Core.Attributes where

import Data.Int (Int64)
import Data.Text (Text)
import OTel.API.Common (AttrVals)
import OTel.API.Common.Internal (Key(..), SchemaURL(..), ToAttrVal(..))

-- | The URL of the OpenTelemetry schema for these keys and values.
pattern RESOURCE_SCHEMA_URL :: SchemaURL
pattern RESOURCE_SCHEMA_URL <- SchemaURL "https://opentelemetry.io/schemas/1.12.0" where
  RESOURCE_SCHEMA_URL = SchemaURL "https://opentelemetry.io/schemas/1.12.0"

{-|
Array of brand name and version separated by a space

[Notes]: This value is intended to be taken from the <https://wicg.github.io/ua-client-hints/#interface> (navigator.userAgentData.brands).
-}
pattern BROWSER_BRANDS :: Key (AttrVals Text)
pattern BROWSER_BRANDS <- Key "browser.brands" where
  BROWSER_BRANDS = Key "browser.brands"

{-|
The platform on which the browser is running

[Notes]: This value is intended to be taken from the <https://wicg.github.io/ua-client-hints/#interface> (navigator.userAgentData.platform). If unavailable, the legacy @navigator.platform@ API SHOULD NOT be used instead and this attribute SHOULD be left unset in order for the values to be consistent.
The list of possible values is defined in the <https://wicg.github.io/ua-client-hints/#sec-ch-ua-platform>. Note that some (but not all) of these values can overlap with values in the <./os.md>. However, for consistency, the values in the @browser.platform@ attribute should capture the exact value that the user agent provides.
-}
pattern BROWSER_PLATFORM :: Key Text
pattern BROWSER_PLATFORM <- Key "browser.platform" where
  BROWSER_PLATFORM = Key "browser.platform"

{-|
Full user-agent string provided by the browser

[Notes]: The user-agent value SHOULD be provided only from browsers that do not have a mechanism to retrieve brands and platform individually from the User-Agent Client Hints API. To retrieve the value, the legacy @navigator.userAgent@ API can be used.
-}
pattern BROWSER_USER_AGENT :: Key Text
pattern BROWSER_USER_AGENT <- Key "browser.user_agent" where
  BROWSER_USER_AGENT = Key "browser.user_agent"

{-|
Name of the cloud provider.
-}
pattern CLOUD_PROVIDER :: Key Text
pattern CLOUD_PROVIDER <- Key "cloud.provider" where
  CLOUD_PROVIDER = Key "cloud.provider"

{-|
The cloud account ID the resource is assigned to.
-}
pattern CLOUD_ACCOUNT_ID :: Key Text
pattern CLOUD_ACCOUNT_ID <- Key "cloud.account.id" where
  CLOUD_ACCOUNT_ID = Key "cloud.account.id"

{-|
The geographical region the resource is running.

[Notes]: Refer to your provider's docs to see the available regions, for example <https://www.alibabacloud.com/help/doc-detail/40654.htm>, <https://aws.amazon.com/about-aws/global-infrastructure/regions_az/>, <https://azure.microsoft.com/en-us/global-infrastructure/geographies/>, <https://cloud.google.com/about/locations>, or <https://intl.cloud.tencent.com/document/product/213/6091>.
-}
pattern CLOUD_REGION :: Key Text
pattern CLOUD_REGION <- Key "cloud.region" where
  CLOUD_REGION = Key "cloud.region"

{-|
Cloud regions often have multiple, isolated locations known as zones to increase availability. Availability zone represents the zone where the resource is running.

[Notes]: Availability zones are called &quot;zones&quot; on Alibaba Cloud and Google Cloud.
-}
pattern CLOUD_AVAILABILITY_ZONE :: Key Text
pattern CLOUD_AVAILABILITY_ZONE <- Key "cloud.availability_zone" where
  CLOUD_AVAILABILITY_ZONE = Key "cloud.availability_zone"

{-|
The cloud platform in use.

[Notes]: The prefix of the service SHOULD match the one specified in @cloud.provider@.
-}
pattern CLOUD_PLATFORM :: Key Text
pattern CLOUD_PLATFORM <- Key "cloud.platform" where
  CLOUD_PLATFORM = Key "cloud.platform"

{-|
The Amazon Resource Name (ARN) of an <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_instances.html>.
-}
pattern AWS_ECS_CONTAINER_ARN :: Key Text
pattern AWS_ECS_CONTAINER_ARN <- Key "aws.ecs.container.arn" where
  AWS_ECS_CONTAINER_ARN = Key "aws.ecs.container.arn"

{-|
The ARN of an <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/clusters.html>.
-}
pattern AWS_ECS_CLUSTER_ARN :: Key Text
pattern AWS_ECS_CLUSTER_ARN <- Key "aws.ecs.cluster.arn" where
  AWS_ECS_CLUSTER_ARN = Key "aws.ecs.cluster.arn"

{-|
The <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html> for an ECS task.
-}
pattern AWS_ECS_LAUNCHTYPE :: Key Text
pattern AWS_ECS_LAUNCHTYPE <- Key "aws.ecs.launchtype" where
  AWS_ECS_LAUNCHTYPE = Key "aws.ecs.launchtype"

{-|
The ARN of an <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html>.
-}
pattern AWS_ECS_TASK_ARN :: Key Text
pattern AWS_ECS_TASK_ARN <- Key "aws.ecs.task.arn" where
  AWS_ECS_TASK_ARN = Key "aws.ecs.task.arn"

{-|
The task definition family this task definition is a member of.
-}
pattern AWS_ECS_TASK_FAMILY :: Key Text
pattern AWS_ECS_TASK_FAMILY <- Key "aws.ecs.task.family" where
  AWS_ECS_TASK_FAMILY = Key "aws.ecs.task.family"

{-|
The revision for this task definition.
-}
pattern AWS_ECS_TASK_REVISION :: Key Text
pattern AWS_ECS_TASK_REVISION <- Key "aws.ecs.task.revision" where
  AWS_ECS_TASK_REVISION = Key "aws.ecs.task.revision"

{-|
The ARN of an EKS cluster.
-}
pattern AWS_EKS_CLUSTER_ARN :: Key Text
pattern AWS_EKS_CLUSTER_ARN <- Key "aws.eks.cluster.arn" where
  AWS_EKS_CLUSTER_ARN = Key "aws.eks.cluster.arn"

{-|
The name(s) of the AWS log group(s) an application is writing to.

[Notes]: Multiple log groups must be supported for cases like multi-container applications, where a single application has sidecar containers, and each write to their own log group.
-}
pattern AWS_LOG_GROUP_NAMES :: Key (AttrVals Text)
pattern AWS_LOG_GROUP_NAMES <- Key "aws.log.group.names" where
  AWS_LOG_GROUP_NAMES = Key "aws.log.group.names"

{-|
The Amazon Resource Name(s) (ARN) of the AWS log group(s).

[Notes]: See the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format>.
-}
pattern AWS_LOG_GROUP_ARNS :: Key (AttrVals Text)
pattern AWS_LOG_GROUP_ARNS <- Key "aws.log.group.arns" where
  AWS_LOG_GROUP_ARNS = Key "aws.log.group.arns"

{-|
The name(s) of the AWS log stream(s) an application is writing to.
-}
pattern AWS_LOG_STREAM_NAMES :: Key (AttrVals Text)
pattern AWS_LOG_STREAM_NAMES <- Key "aws.log.stream.names" where
  AWS_LOG_STREAM_NAMES = Key "aws.log.stream.names"

{-|
The ARN(s) of the AWS log stream(s).

[Notes]: See the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format>. One log group can contain several log streams, so these ARNs necessarily identify both a log group and a log stream.
-}
pattern AWS_LOG_STREAM_ARNS :: Key (AttrVals Text)
pattern AWS_LOG_STREAM_ARNS <- Key "aws.log.stream.arns" where
  AWS_LOG_STREAM_ARNS = Key "aws.log.stream.arns"

{-|
Container name used by container runtime.
-}
pattern CONTAINER_NAME :: Key Text
pattern CONTAINER_NAME <- Key "container.name" where
  CONTAINER_NAME = Key "container.name"

{-|
Container ID. Usually a UUID, as for example used to <https://docs.docker.com/engine/reference/run/#container-identification>. The UUID might be abbreviated.
-}
pattern CONTAINER_ID :: Key Text
pattern CONTAINER_ID <- Key "container.id" where
  CONTAINER_ID = Key "container.id"

{-|
The container runtime managing this container.
-}
pattern CONTAINER_RUNTIME :: Key Text
pattern CONTAINER_RUNTIME <- Key "container.runtime" where
  CONTAINER_RUNTIME = Key "container.runtime"

{-|
Name of the image the container was built on.
-}
pattern CONTAINER_IMAGE_NAME :: Key Text
pattern CONTAINER_IMAGE_NAME <- Key "container.image.name" where
  CONTAINER_IMAGE_NAME = Key "container.image.name"

{-|
Container image tag.
-}
pattern CONTAINER_IMAGE_TAG :: Key Text
pattern CONTAINER_IMAGE_TAG <- Key "container.image.tag" where
  CONTAINER_IMAGE_TAG = Key "container.image.tag"

{-|
Name of the <https://en.wikipedia.org/wiki/Deployment_environment> (aka deployment tier).
-}
pattern DEPLOYMENT_ENVIRONMENT :: Key Text
pattern DEPLOYMENT_ENVIRONMENT <- Key "deployment.environment" where
  DEPLOYMENT_ENVIRONMENT = Key "deployment.environment"

{-|
A unique identifier representing the device

[Notes]: The device identifier MUST only be defined using the values outlined below. This value is not an advertising identifier and MUST NOT be used as such. On iOS (Swift or Objective-C), this value MUST be equal to the <https://developer.apple.com/documentation/uikit/uidevice/1620059-identifierforvendor>. On Android (Java or Kotlin), this value MUST be equal to the Firebase Installation ID or a globally unique UUID which is persisted across sessions in your application. More information can be found <https://developer.android.com/training/articles/user-data-ids> on best practices and exact implementation details. Caution should be taken when storing personal data or anything which can identify a user. GDPR and data protection laws may apply, ensure you do your own due diligence.
-}
pattern DEVICE_ID :: Key Text
pattern DEVICE_ID <- Key "device.id" where
  DEVICE_ID = Key "device.id"

{-|
The model identifier for the device

[Notes]: It's recommended this value represents a machine readable version of the model identifier rather than the market or consumer-friendly name of the device.
-}
pattern DEVICE_MODEL_IDENTIFIER :: Key Text
pattern DEVICE_MODEL_IDENTIFIER <- Key "device.model.identifier" where
  DEVICE_MODEL_IDENTIFIER = Key "device.model.identifier"

{-|
The marketing name for the device model

[Notes]: It's recommended this value represents a human readable version of the device model rather than a machine readable alternative.
-}
pattern DEVICE_MODEL_NAME :: Key Text
pattern DEVICE_MODEL_NAME <- Key "device.model.name" where
  DEVICE_MODEL_NAME = Key "device.model.name"

{-|
The name of the device manufacturer

[Notes]: The Android OS provides this field via <https://developer.android.com/reference/android/os/Build#MANUFACTURER>. iOS apps SHOULD hardcode the value @Apple@.
-}
pattern DEVICE_MANUFACTURER :: Key Text
pattern DEVICE_MANUFACTURER <- Key "device.manufacturer" where
  DEVICE_MANUFACTURER = Key "device.manufacturer"

{-|
The name of the single function that this runtime instance executes.

[Notes]: This is the name of the function as configured/deployed on the FaaS
platform and is usually different from the name of the callback
function (which may be stored in the
<../../trace/semantic_conventions/span-general.md#source-code-attributes>
span attributes).For some cloud providers, the above definition is ambiguous. The following
definition of function name MUST be used for this attribute
(and consequently the span name) for the listed cloud providers/products:<li><strong>Azure:</strong>  The full name @<FUNCAPP>\/<FUNC>@, i.e., function app name
followed by a forward slash followed by the function name (this form
can also be seen in the resource JSON for the function).
This means that a span attribute MUST be used, as an Azure function
app can host multiple functions that would usually share
a TracerProvider (see also the @faas.id@ attribute).</li>

-}
pattern FAAS_NAME :: Key Text
pattern FAAS_NAME <- Key "faas.name" where
  FAAS_NAME = Key "faas.name"

{-|
The unique ID of the single function that this runtime instance executes.

[Notes]: On some cloud providers, it may not be possible to determine the full ID at startup,
so consider setting @faas.id@ as a span attribute instead.The exact value to use for @faas.id@ depends on the cloud provider:<li><strong>AWS Lambda:</strong> The function <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html>.
Take care not to use the &quot;invoked ARN&quot; directly but replace any
<https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html>
with the resolved function version, as the same runtime instance may be invokable with
multiple different aliases.</li>
<li><strong>GCP:</strong> The <https://cloud.google.com/iam/docs/full-resource-names></li>
<li><strong>Azure:</strong> The <https://docs.microsoft.com/en-us/rest/api/resources/resources/get-by-id> of the invoked function,
<em>not</em> the function app, having the form
@\/subscriptions\/<SUBSCIPTION_GUID>\/resourceGroups\/<RG>\/providers\/Microsoft.Web\/sites\/<FUNCAPP>\/functions\/<FUNC>@.
This means that a span attribute MUST be used, as an Azure function app can host multiple functions that would usually share
a TracerProvider.</li>

-}
pattern FAAS_ID :: Key Text
pattern FAAS_ID <- Key "faas.id" where
  FAAS_ID = Key "faas.id"

{-|
The immutable version of the function being executed.

[Notes]: Depending on the cloud provider and platform, use:<li><strong>AWS Lambda:</strong> The <https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html>
(an integer represented as a decimal string).</li>
<li><strong>Google Cloud Run:</strong> The <https://cloud.google.com/run/docs/managing/revisions>
(i.e., the function name plus the revision suffix).</li>
<li><strong>Google Cloud Functions:</strong> The value of the
<https://cloud.google.com/functions/docs/env-var#runtime_environment_variables_set_automatically>.</li>
<li><strong>Azure Functions:</strong> Not applicable. Do not set this attribute.</li>

-}
pattern FAAS_VERSION :: Key Text
pattern FAAS_VERSION <- Key "faas.version" where
  FAAS_VERSION = Key "faas.version"

{-|
The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version.

[Notes]: <li><strong>AWS Lambda:</strong> Use the (full) log stream name.</li>

-}
pattern FAAS_INSTANCE :: Key Text
pattern FAAS_INSTANCE <- Key "faas.instance" where
  FAAS_INSTANCE = Key "faas.instance"

{-|
The amount of memory available to the serverless function in MiB.

[Notes]: It's recommended to set this attribute since e.g. too little memory can easily stop a Java AWS Lambda function from working correctly. On AWS Lambda, the environment variable @AWS_LAMBDA_FUNCTION_MEMORY_SIZE@ provides this information.
-}
pattern FAAS_MAX_MEMORY :: Key Int64
pattern FAAS_MAX_MEMORY <- Key "faas.max_memory" where
  FAAS_MAX_MEMORY = Key "faas.max_memory"

{-|
Unique host ID. For Cloud, this must be the instance_id assigned by the cloud provider.
-}
pattern HOST_ID :: Key Text
pattern HOST_ID <- Key "host.id" where
  HOST_ID = Key "host.id"

{-|
Name of the host. On Unix systems, it may contain what the hostname command returns, or the fully qualified hostname, or another name specified by the user.
-}
pattern HOST_NAME :: Key Text
pattern HOST_NAME <- Key "host.name" where
  HOST_NAME = Key "host.name"

{-|
Type of host. For Cloud, this must be the machine type.
-}
pattern HOST_TYPE :: Key Text
pattern HOST_TYPE <- Key "host.type" where
  HOST_TYPE = Key "host.type"

{-|
The CPU architecture the host system is running on.
-}
pattern HOST_ARCH :: Key Text
pattern HOST_ARCH <- Key "host.arch" where
  HOST_ARCH = Key "host.arch"

{-|
Name of the VM image or OS install the host was instantiated from.
-}
pattern HOST_IMAGE_NAME :: Key Text
pattern HOST_IMAGE_NAME <- Key "host.image.name" where
  HOST_IMAGE_NAME = Key "host.image.name"

{-|
VM image ID. For Cloud, this value is from the provider.
-}
pattern HOST_IMAGE_ID :: Key Text
pattern HOST_IMAGE_ID <- Key "host.image.id" where
  HOST_IMAGE_ID = Key "host.image.id"

{-|
The version string of the VM image as defined in <README.md#version-attributes>.
-}
pattern HOST_IMAGE_VERSION :: Key Text
pattern HOST_IMAGE_VERSION <- Key "host.image.version" where
  HOST_IMAGE_VERSION = Key "host.image.version"

{-|
The name of the cluster.
-}
pattern K8S_CLUSTER_NAME :: Key Text
pattern K8S_CLUSTER_NAME <- Key "k8s.cluster.name" where
  K8S_CLUSTER_NAME = Key "k8s.cluster.name"

{-|
The name of the Node.
-}
pattern K8S_NODE_NAME :: Key Text
pattern K8S_NODE_NAME <- Key "k8s.node.name" where
  K8S_NODE_NAME = Key "k8s.node.name"

{-|
The UID of the Node.
-}
pattern K8S_NODE_UID :: Key Text
pattern K8S_NODE_UID <- Key "k8s.node.uid" where
  K8S_NODE_UID = Key "k8s.node.uid"

{-|
The name of the namespace that the pod is running in.
-}
pattern K8S_NAMESPACE_NAME :: Key Text
pattern K8S_NAMESPACE_NAME <- Key "k8s.namespace.name" where
  K8S_NAMESPACE_NAME = Key "k8s.namespace.name"

{-|
The UID of the Pod.
-}
pattern K8S_POD_UID :: Key Text
pattern K8S_POD_UID <- Key "k8s.pod.uid" where
  K8S_POD_UID = Key "k8s.pod.uid"

{-|
The name of the Pod.
-}
pattern K8S_POD_NAME :: Key Text
pattern K8S_POD_NAME <- Key "k8s.pod.name" where
  K8S_POD_NAME = Key "k8s.pod.name"

{-|
The name of the Container from Pod specification, must be unique within a Pod. Container runtime usually uses different globally unique name (@container.name@).
-}
pattern K8S_CONTAINER_NAME :: Key Text
pattern K8S_CONTAINER_NAME <- Key "k8s.container.name" where
  K8S_CONTAINER_NAME = Key "k8s.container.name"

{-|
Number of times the container was restarted. This attribute can be used to identify a particular container (running or stopped) within a container spec.
-}
pattern K8S_CONTAINER_RESTART_COUNT :: Key Int64
pattern K8S_CONTAINER_RESTART_COUNT <- Key "k8s.container.restart_count" where
  K8S_CONTAINER_RESTART_COUNT = Key "k8s.container.restart_count"

{-|
The UID of the ReplicaSet.
-}
pattern K8S_REPLICASET_UID :: Key Text
pattern K8S_REPLICASET_UID <- Key "k8s.replicaset.uid" where
  K8S_REPLICASET_UID = Key "k8s.replicaset.uid"

{-|
The name of the ReplicaSet.
-}
pattern K8S_REPLICASET_NAME :: Key Text
pattern K8S_REPLICASET_NAME <- Key "k8s.replicaset.name" where
  K8S_REPLICASET_NAME = Key "k8s.replicaset.name"

{-|
The UID of the Deployment.
-}
pattern K8S_DEPLOYMENT_UID :: Key Text
pattern K8S_DEPLOYMENT_UID <- Key "k8s.deployment.uid" where
  K8S_DEPLOYMENT_UID = Key "k8s.deployment.uid"

{-|
The name of the Deployment.
-}
pattern K8S_DEPLOYMENT_NAME :: Key Text
pattern K8S_DEPLOYMENT_NAME <- Key "k8s.deployment.name" where
  K8S_DEPLOYMENT_NAME = Key "k8s.deployment.name"

{-|
The UID of the StatefulSet.
-}
pattern K8S_STATEFULSET_UID :: Key Text
pattern K8S_STATEFULSET_UID <- Key "k8s.statefulset.uid" where
  K8S_STATEFULSET_UID = Key "k8s.statefulset.uid"

{-|
The name of the StatefulSet.
-}
pattern K8S_STATEFULSET_NAME :: Key Text
pattern K8S_STATEFULSET_NAME <- Key "k8s.statefulset.name" where
  K8S_STATEFULSET_NAME = Key "k8s.statefulset.name"

{-|
The UID of the DaemonSet.
-}
pattern K8S_DAEMONSET_UID :: Key Text
pattern K8S_DAEMONSET_UID <- Key "k8s.daemonset.uid" where
  K8S_DAEMONSET_UID = Key "k8s.daemonset.uid"

{-|
The name of the DaemonSet.
-}
pattern K8S_DAEMONSET_NAME :: Key Text
pattern K8S_DAEMONSET_NAME <- Key "k8s.daemonset.name" where
  K8S_DAEMONSET_NAME = Key "k8s.daemonset.name"

{-|
The UID of the Job.
-}
pattern K8S_JOB_UID :: Key Text
pattern K8S_JOB_UID <- Key "k8s.job.uid" where
  K8S_JOB_UID = Key "k8s.job.uid"

{-|
The name of the Job.
-}
pattern K8S_JOB_NAME :: Key Text
pattern K8S_JOB_NAME <- Key "k8s.job.name" where
  K8S_JOB_NAME = Key "k8s.job.name"

{-|
The UID of the CronJob.
-}
pattern K8S_CRONJOB_UID :: Key Text
pattern K8S_CRONJOB_UID <- Key "k8s.cronjob.uid" where
  K8S_CRONJOB_UID = Key "k8s.cronjob.uid"

{-|
The name of the CronJob.
-}
pattern K8S_CRONJOB_NAME :: Key Text
pattern K8S_CRONJOB_NAME <- Key "k8s.cronjob.name" where
  K8S_CRONJOB_NAME = Key "k8s.cronjob.name"

{-|
The operating system type.
-}
pattern OS_TYPE :: Key Text
pattern OS_TYPE <- Key "os.type" where
  OS_TYPE = Key "os.type"

{-|
Human readable (not intended to be parsed) OS version information, like e.g. reported by @ver@ or @lsb_release -a@ commands.
-}
pattern OS_DESCRIPTION :: Key Text
pattern OS_DESCRIPTION <- Key "os.description" where
  OS_DESCRIPTION = Key "os.description"

{-|
Human readable operating system name.
-}
pattern OS_NAME :: Key Text
pattern OS_NAME <- Key "os.name" where
  OS_NAME = Key "os.name"

{-|
The version string of the operating system as defined in <../../resource/semantic_conventions/README.md#version-attributes>.
-}
pattern OS_VERSION :: Key Text
pattern OS_VERSION <- Key "os.version" where
  OS_VERSION = Key "os.version"

{-|
Process identifier (PID).
-}
pattern PROCESS_PID :: Key Int64
pattern PROCESS_PID <- Key "process.pid" where
  PROCESS_PID = Key "process.pid"

{-|
The name of the process executable. On Linux based systems, can be set to the @Name@ in @proc\/[pid]\/status@. On Windows, can be set to the base name of @GetProcessImageFileNameW@.
-}
pattern PROCESS_EXECUTABLE_NAME :: Key Text
pattern PROCESS_EXECUTABLE_NAME <- Key "process.executable.name" where
  PROCESS_EXECUTABLE_NAME = Key "process.executable.name"

{-|
The full path to the process executable. On Linux based systems, can be set to the target of @proc\/[pid]\/exe@. On Windows, can be set to the result of @GetProcessImageFileNameW@.
-}
pattern PROCESS_EXECUTABLE_PATH :: Key Text
pattern PROCESS_EXECUTABLE_PATH <- Key "process.executable.path" where
  PROCESS_EXECUTABLE_PATH = Key "process.executable.path"

{-|
The command used to launch the process (i.e. the command name). On Linux based systems, can be set to the zeroth string in @proc\/[pid]\/cmdline@. On Windows, can be set to the first parameter extracted from @GetCommandLineW@.
-}
pattern PROCESS_COMMAND :: Key Text
pattern PROCESS_COMMAND <- Key "process.command" where
  PROCESS_COMMAND = Key "process.command"

{-|
The full command used to launch the process as a single string representing the full command. On Windows, can be set to the result of @GetCommandLineW@. Do not set this if you have to assemble it just for monitoring; use @process.command_args@ instead.
-}
pattern PROCESS_COMMAND_LINE :: Key Text
pattern PROCESS_COMMAND_LINE <- Key "process.command_line" where
  PROCESS_COMMAND_LINE = Key "process.command_line"

{-|
All the command arguments (including the command/executable itself) as received by the process. On Linux-based systems (and some other Unixoid systems supporting procfs), can be set according to the list of null-delimited strings extracted from @proc\/[pid]\/cmdline@. For libc-based executables, this would be the full argv vector passed to @main@.
-}
pattern PROCESS_COMMAND_ARGS :: Key (AttrVals Text)
pattern PROCESS_COMMAND_ARGS <- Key "process.command_args" where
  PROCESS_COMMAND_ARGS = Key "process.command_args"

{-|
The username of the user that owns the process.
-}
pattern PROCESS_OWNER :: Key Text
pattern PROCESS_OWNER <- Key "process.owner" where
  PROCESS_OWNER = Key "process.owner"

{-|
The name of the runtime of this process. For compiled native binaries, this SHOULD be the name of the compiler.
-}
pattern PROCESS_RUNTIME_NAME :: Key Text
pattern PROCESS_RUNTIME_NAME <- Key "process.runtime.name" where
  PROCESS_RUNTIME_NAME = Key "process.runtime.name"

{-|
The version of the runtime of this process, as returned by the runtime without modification.
-}
pattern PROCESS_RUNTIME_VERSION :: Key Text
pattern PROCESS_RUNTIME_VERSION <- Key "process.runtime.version" where
  PROCESS_RUNTIME_VERSION = Key "process.runtime.version"

{-|
An additional description about the runtime of the process, for example a specific vendor customization of the runtime environment.
-}
pattern PROCESS_RUNTIME_DESCRIPTION :: Key Text
pattern PROCESS_RUNTIME_DESCRIPTION <- Key "process.runtime.description" where
  PROCESS_RUNTIME_DESCRIPTION = Key "process.runtime.description"

{-|
Logical name of the service.

[Notes]: MUST be the same for all instances of horizontally scaled services. If the value was not specified, SDKs MUST fallback to @unknown_service:@ concatenated with <process.md#process>, e.g. @unknown_service:bash@. If @process.executable.name@ is not available, the value MUST be set to @unknown_service@.
-}
pattern SERVICE_NAME :: Key Text
pattern SERVICE_NAME <- Key "service.name" where
  SERVICE_NAME = Key "service.name"

{-|
A namespace for @service.name@.

[Notes]: A string value having a meaning that helps to distinguish a group of services, for example the team name that owns a group of services. @service.name@ is expected to be unique within the same namespace. If @service.namespace@ is not specified in the Resource then @service.name@ is expected to be unique for all services that have no explicit namespace defined (so the empty/unspecified namespace is simply one more valid namespace). Zero-length namespace string is assumed equal to unspecified namespace.
-}
pattern SERVICE_NAMESPACE :: Key Text
pattern SERVICE_NAMESPACE <- Key "service.namespace" where
  SERVICE_NAMESPACE = Key "service.namespace"

{-|
The string ID of the service instance.

[Notes]: MUST be unique for each instance of the same @service.namespace,service.name@ pair (in other words @service.namespace,service.name,service.instance.id@ triplet MUST be globally unique). The ID helps to distinguish instances of the same service that exist at the same time (e.g. instances of a horizontally scaled service). It is preferable for the ID to be persistent and stay the same for the lifetime of the service instance, however it is acceptable that the ID is ephemeral and changes during important lifetime events for the service (e.g. service restarts). If the service has no inherent unique ID that can be used as the value of this attribute it is recommended to generate a random Version 1 or Version 4 RFC 4122 UUID (services aiming for reproducible UUIDs may also use Version 5, see RFC 4122 for more recommendations).
-}
pattern SERVICE_INSTANCE_ID :: Key Text
pattern SERVICE_INSTANCE_ID <- Key "service.instance.id" where
  SERVICE_INSTANCE_ID = Key "service.instance.id"

{-|
The version string of the service API or implementation.
-}
pattern SERVICE_VERSION :: Key Text
pattern SERVICE_VERSION <- Key "service.version" where
  SERVICE_VERSION = Key "service.version"

{-|
The name of the telemetry SDK as defined above.
-}
pattern TELEMETRY_SDK_NAME :: Key Text
pattern TELEMETRY_SDK_NAME <- Key "telemetry.sdk.name" where
  TELEMETRY_SDK_NAME = Key "telemetry.sdk.name"

{-|
The language of the telemetry SDK.
-}
pattern TELEMETRY_SDK_LANGUAGE :: Key Text
pattern TELEMETRY_SDK_LANGUAGE <- Key "telemetry.sdk.language" where
  TELEMETRY_SDK_LANGUAGE = Key "telemetry.sdk.language"

{-|
The version string of the telemetry SDK.
-}
pattern TELEMETRY_SDK_VERSION :: Key Text
pattern TELEMETRY_SDK_VERSION <- Key "telemetry.sdk.version" where
  TELEMETRY_SDK_VERSION = Key "telemetry.sdk.version"

{-|
The version string of the auto instrumentation agent, if used.
-}
pattern TELEMETRY_AUTO_VERSION :: Key Text
pattern TELEMETRY_AUTO_VERSION <- Key "telemetry.auto.version" where
  TELEMETRY_AUTO_VERSION = Key "telemetry.auto.version"

{-|
The name of the web engine.
-}
pattern WEBENGINE_NAME :: Key Text
pattern WEBENGINE_NAME <- Key "webengine.name" where
  WEBENGINE_NAME = Key "webengine.name"

{-|
The version of the web engine.
-}
pattern WEBENGINE_VERSION :: Key Text
pattern WEBENGINE_VERSION <- Key "webengine.version" where
  WEBENGINE_VERSION = Key "webengine.version"

{-|
Additional description of the web engine (e.g. detailed version and edition information).
-}
pattern WEBENGINE_DESCRIPTION :: Key Text
pattern WEBENGINE_DESCRIPTION <- Key "webengine.description" where
  WEBENGINE_DESCRIPTION = Key "webengine.description"
data CloudProvider
  = CloudProviderAlibabaCloud -- ^ Alibaba Cloud.
  | CloudProviderAws -- ^ Amazon Web Services.
  | CloudProviderAzure -- ^ Microsoft Azure.
  | CloudProviderGcp -- ^ Google Cloud Platform.
  | CloudProviderTencentCloud -- ^ Tencent Cloud.

instance ToAttrVal CloudProvider Text where
  toAttrVal = \case
    CloudProviderAlibabaCloud -> "alibaba_cloud"
    CloudProviderAws -> "aws"
    CloudProviderAzure -> "azure"
    CloudProviderGcp -> "gcp"
    CloudProviderTencentCloud -> "tencent_cloud"

data CloudPlatform
  = CloudPlatformAlibabaCloudEcs -- ^ Alibaba Cloud Elastic Compute Service.
  | CloudPlatformAlibabaCloudFc -- ^ Alibaba Cloud Function Compute.
  | CloudPlatformAwsEc2 -- ^ AWS Elastic Compute Cloud.
  | CloudPlatformAwsEcs -- ^ AWS Elastic Container Service.
  | CloudPlatformAwsEks -- ^ AWS Elastic Kubernetes Service.
  | CloudPlatformAwsLambda -- ^ AWS Lambda.
  | CloudPlatformAwsElasticBeanstalk -- ^ AWS Elastic Beanstalk.
  | CloudPlatformAwsAppRunner -- ^ AWS App Runner.
  | CloudPlatformAzureVm -- ^ Azure Virtual Machines.
  | CloudPlatformAzureContainerInstances -- ^ Azure Container Instances.
  | CloudPlatformAzureAks -- ^ Azure Kubernetes Service.
  | CloudPlatformAzureFunctions -- ^ Azure Functions.
  | CloudPlatformAzureAppService -- ^ Azure App Service.
  | CloudPlatformGcpComputeEngine -- ^ Google Cloud Compute Engine (GCE).
  | CloudPlatformGcpCloudRun -- ^ Google Cloud Run.
  | CloudPlatformGcpKubernetesEngine -- ^ Google Cloud Kubernetes Engine (GKE).
  | CloudPlatformGcpCloudFunctions -- ^ Google Cloud Functions (GCF).
  | CloudPlatformGcpAppEngine -- ^ Google Cloud App Engine (GAE).
  | CloudPlatformTencentCloudCvm -- ^ Tencent Cloud Cloud Virtual Machine (CVM).
  | CloudPlatformTencentCloudEks -- ^ Tencent Cloud Elastic Kubernetes Service (EKS).
  | CloudPlatformTencentCloudScf -- ^ Tencent Cloud Serverless Cloud Function (SCF).

instance ToAttrVal CloudPlatform Text where
  toAttrVal = \case
    CloudPlatformAlibabaCloudEcs -> "alibaba_cloud_ecs"
    CloudPlatformAlibabaCloudFc -> "alibaba_cloud_fc"
    CloudPlatformAwsEc2 -> "aws_ec2"
    CloudPlatformAwsEcs -> "aws_ecs"
    CloudPlatformAwsEks -> "aws_eks"
    CloudPlatformAwsLambda -> "aws_lambda"
    CloudPlatformAwsElasticBeanstalk -> "aws_elastic_beanstalk"
    CloudPlatformAwsAppRunner -> "aws_app_runner"
    CloudPlatformAzureVm -> "azure_vm"
    CloudPlatformAzureContainerInstances -> "azure_container_instances"
    CloudPlatformAzureAks -> "azure_aks"
    CloudPlatformAzureFunctions -> "azure_functions"
    CloudPlatformAzureAppService -> "azure_app_service"
    CloudPlatformGcpComputeEngine -> "gcp_compute_engine"
    CloudPlatformGcpCloudRun -> "gcp_cloud_run"
    CloudPlatformGcpKubernetesEngine -> "gcp_kubernetes_engine"
    CloudPlatformGcpCloudFunctions -> "gcp_cloud_functions"
    CloudPlatformGcpAppEngine -> "gcp_app_engine"
    CloudPlatformTencentCloudCvm -> "tencent_cloud_cvm"
    CloudPlatformTencentCloudEks -> "tencent_cloud_eks"
    CloudPlatformTencentCloudScf -> "tencent_cloud_scf"

data AwsEcsLaunchtype
  = AwsEcsLaunchtypeEc2 -- ^ ec2.
  | AwsEcsLaunchtypeFargate -- ^ fargate.

instance ToAttrVal AwsEcsLaunchtype Text where
  toAttrVal = \case
    AwsEcsLaunchtypeEc2 -> "ec2"
    AwsEcsLaunchtypeFargate -> "fargate"

data HostArch
  = HostArchAmd64 -- ^ AMD64.
  | HostArchArm32 -- ^ ARM32.
  | HostArchArm64 -- ^ ARM64.
  | HostArchIa64 -- ^ Itanium.
  | HostArchPpc32 -- ^ 32-bit PowerPC.
  | HostArchPpc64 -- ^ 64-bit PowerPC.
  | HostArchS390x -- ^ IBM z/Architecture.
  | HostArchX86 -- ^ 32-bit x86.

instance ToAttrVal HostArch Text where
  toAttrVal = \case
    HostArchAmd64 -> "amd64"
    HostArchArm32 -> "arm32"
    HostArchArm64 -> "arm64"
    HostArchIa64 -> "ia64"
    HostArchPpc32 -> "ppc32"
    HostArchPpc64 -> "ppc64"
    HostArchS390x -> "s390x"
    HostArchX86 -> "x86"

data OsType
  = OsTypeWindows -- ^ Microsoft Windows.
  | OsTypeLinux -- ^ Linux.
  | OsTypeDarwin -- ^ Apple Darwin.
  | OsTypeFreebsd -- ^ FreeBSD.
  | OsTypeNetbsd -- ^ NetBSD.
  | OsTypeOpenbsd -- ^ OpenBSD.
  | OsTypeDragonflybsd -- ^ DragonFly BSD.
  | OsTypeHpux -- ^ HP-UX (Hewlett Packard Unix).
  | OsTypeAix -- ^ AIX (Advanced Interactive eXecutive).
  | OsTypeSolaris -- ^ SunOS, Oracle Solaris.
  | OsTypeZOs -- ^ IBM z/OS.

instance ToAttrVal OsType Text where
  toAttrVal = \case
    OsTypeWindows -> "windows"
    OsTypeLinux -> "linux"
    OsTypeDarwin -> "darwin"
    OsTypeFreebsd -> "freebsd"
    OsTypeNetbsd -> "netbsd"
    OsTypeOpenbsd -> "openbsd"
    OsTypeDragonflybsd -> "dragonflybsd"
    OsTypeHpux -> "hpux"
    OsTypeAix -> "aix"
    OsTypeSolaris -> "solaris"
    OsTypeZOs -> "z_os"

data TelemetrySdkLanguage
  = TelemetrySdkLanguageCpp -- ^ cpp.
  | TelemetrySdkLanguageDotnet -- ^ dotnet.
  | TelemetrySdkLanguageErlang -- ^ erlang.
  | TelemetrySdkLanguageGo -- ^ go.
  | TelemetrySdkLanguageJava -- ^ java.
  | TelemetrySdkLanguageNodejs -- ^ nodejs.
  | TelemetrySdkLanguagePhp -- ^ php.
  | TelemetrySdkLanguagePython -- ^ python.
  | TelemetrySdkLanguageRuby -- ^ ruby.
  | TelemetrySdkLanguageWebjs -- ^ webjs.
  | TelemetrySdkLanguageSwift -- ^ swift.

instance ToAttrVal TelemetrySdkLanguage Text where
  toAttrVal = \case
    TelemetrySdkLanguageCpp -> "cpp"
    TelemetrySdkLanguageDotnet -> "dotnet"
    TelemetrySdkLanguageErlang -> "erlang"
    TelemetrySdkLanguageGo -> "go"
    TelemetrySdkLanguageJava -> "java"
    TelemetrySdkLanguageNodejs -> "nodejs"
    TelemetrySdkLanguagePhp -> "php"
    TelemetrySdkLanguagePython -> "python"
    TelemetrySdkLanguageRuby -> "ruby"
    TelemetrySdkLanguageWebjs -> "webjs"
    TelemetrySdkLanguageSwift -> "swift"
