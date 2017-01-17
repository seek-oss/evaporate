## Evaporate
[![Build Status](https://travis-ci.org/seek-oss/evaporate.svg?branch=master)](https://travis-ci.org/seek-oss/evaporate)

Evaporate is a simple CloudFormation Stack deployment tool.

It was created to optimise a continuous delivery build & deploy service for
infrastructure and applications, but also to provide a smooth experience for
experimental stack deployments into sandbox/test environments from a workstation
rather than a build/deployment server.

## Installation

### Docker

Pull the latest docker image from `seek/evaporate`.

### Binary

Download the latest binary from the
[releases page](https://github.com/seek-oss/evaporate/releases).

### From source

1. Ensure [The Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
is installed

2. Git clone https://github.com/seek-oss/evaporate to a temp location

3. From within the evaporate repository, execute the following:

  ```
  > stack setup
  > stack install
  ```

4. (Optional) If you've never used The Haskell Stack before you may need to
include its default bin path into your `$PATH`.

## Configuration

### Region

The deployment region is read from the environment variable `AWS_REGION` or
retrieved from the [Instance Identity Document](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-identity-documents.html),
otherwise it defaults to `us-east-1`.

### evaporate.yaml

By default, evaporate looks for a configuration file called `evaporate.yaml`.
This file contains the information required for evaporate to operate on your
stacks. If you want evaporate to look for a different file, append the file path
to the end of your evaporate command:

```
> evaporate upsert myconfig.yaml
```

Each stack to be deployed must be described in `evaporate.yaml` by a stack
definition. A stack definition consists of the following:

1. A stack name
1. A path to a json stack template
1. The parameters specified in the stack template file (separated by AWS account ID)
1. (Optional) Up to 10 tags as key value pairs
   (keys can be 127 characters max, values can be 255 characters max)
1. (Optional) Files to upload to buckets created within previous stacks
1. (Optional) A list of Capabilities needed by the stack, e.g., CAPABILITY_IAM

*NB:* The order of execution is determined based on stack output references.
If there is a cyclic dependency within the stacks (e.g. stack2 references an
output from stack1 and stack1 references an output from stack 2) then the given
command will not be executed on any stacks. If there is no dependency cycle,
the command will be executed on the stacks in an order that ensures that any
stack output references can be resolved when their corresponding stacks are
created/upserted.

*NB:* Only one `evaporate.yaml` file should be needed for each deployment.

See [example/evaporate.yaml](/example/evaporate.yaml).

#### Stack Templates

Each stack definition requires a stack template as a value to the
`template-path` key. The same stack template for can be used for different
stacks.

If multiple stack templates are being used, be sure to add an extra `-v` flag to
the `docker run` command:
```
-v $(pwd)/template-folder/another-template.json:/app/another-template.json
```

#### Parameters

Parameters are separated by AWS account ID. This is done to restrict deployment
to specific accounts. Before executing any commands, the account ID you are
authenticated to is checked against the account IDs in `evaporate.yaml`. If the
account ID is not found, no commands will be executed.

In the case where your stack doesn't require any parameters, then for each AWS
Account ID key make its corresponding value an empty dictionary:
```
123456789999: {}
```
Do not do this:
```
parameters: {}
```

#### Tags

Tags are used to manage AWS resources. They are simply key-value pairs that are
attached to a resource. Tags are specified in each stack definition as a
dictionary under the `tags` key:

```
tags:
  Owner: Admin
  Environment: Sandbox
```

Tags are a completely optional field; this can be reflected by making the `tags`
key have an empty dictionary as a value:

```
tags: {}
```

or just completely omitting the `tags` key altogether.

#### S3 Bucket File Upload

*NB:*
1. File uploads are done before stack creation.
2. This section does not create a bucket itself, it is only used to upload files
to buckets that have already been created within stacks that already exist.

Uploading files/folders to a bucket created in another stack is done by adding
the `s3upload` key to a stack definition and specifying the name of each bucket
as well as the files/folders to be uploaded.

```
s3upload:
- bucket-name: bucket1
  paths:
    folder/file.txt: folder/anotherFolder/file.txt
```

If the key is a file, then the value will be the path the file will appear at in
the bucket. If the key is a folder, then the value will be the path to that
folder in the bucket. For example:

```
paths:
  folder: myFolder
```

This would upload all the files within `folder` but they would be under the
`myFolder` folder in the bucket.

This is useful for altering the structure of files in the bucket without having
to mirror that structure locally. The value may also be left empty:

```
paths:
  path/file.txt:
```

Alternatively, if all files are to have their paths unchanged in the bucket,
then they can described in a list instead of a dictionary of keys with empty
values:

```
paths:
  - file1.txt
  - file2.txt
  - folder1/folder2
```

Bucket names can be those set in a stack template file, or a stack output from
another stack. See [External Values](#external-values) for more information on
this syntax.

```
s3upload:
- bucket-name: ${stack.my-stack.output.BucketName}
  files:
    - file.txt
```

#### Hashing Files

Often you will want to trigger CFN updates when the contents of configuration
files referenced by a stack change, even if the stack template itself has not
changed. An easy way to achieve this is by prefixing the file paths with the
hash of the contents.

The hash is generated by running the SHA1 hashing function across each file
specified, or in the case of a folder, each item in that folder.

This is done by specifying either true or false to the `hash` key for each
bucket within `s3upload`. For example, if you want to upload 3 files and a
folder to the same bucket but only want to hash 2 of the files and the folder,
it would look like this:
```
s3upload:
- bucket-name: bucket1
  hash: true
  paths:
    - file1.txt
    - file2.txt
    - myFolder
- bucket-name: bucket1
  hash: false
  paths:
    - file3.txt
```

The result of this upload would be the following structure within the bucket:
```
/hashOfFile1/file1.txt
/hashOfFile2/file2.txt
/hashOfMyFolder/myFolder/contentsOfMyFolder...
/file3.txt
```

Files are *not* hashed by default so the `hash` key may be set to `false` or
omitted.

File/folder hashes can also be used as parameter values:

```
parameters:
  123412341234:
    FileHash: ${hash.path/to/file.txt}
```

Hashes of files/folders can only be referenced as parameters within the same
stack definition that contains the `s3upload` key that declares them for
uploading.

#### Zipping files

When defining a Lambda in a stack template, code may be inlined, but for
anything more than a few lines this is impractical. By specifying the `zip` flag
(similar to the `hash` flag), each file or folder will be zipped before being
uploaded to the bucket. The Lambda can now point to a zip file containing the
source code files instead.

For example:
```
s3upload:
- bucket-name: lambdaBucket
  zip: true
  paths:
    - sourceFile1.py
    - sourceFolder1
- bucket-name: lambdaBucket
  zip: true
  paths:
    - path/to/sourceFile2.py
    - path/to/sourceFolder2
```

would result in the following files being uploaded to the root folder of the
`lambdaBucket` s3 bucket:
```
sourceFile1.py.zip
  - sourceFile1.py
sourceFolder1.zip
  - contents of sourceFolder1
sourceFile2.py.zip
  - sourceFile2.py
sourceFolder2.zip
  - contents of sourceFolder2
```

Both the `hash` and `zip` flag can be used together. This will yield the same
result as having just the `zip` flag, except all file paths will be
prefixed with the hash of their contents.

*NB:* The `.zip` suffix will be added to the alternate file path so there is no
need to explicitly name the alternate path `path/to/file.zip` as this will
result in a file called `file.zip.zip`.

#### Capabilities

If a CFN stack requires [Capabilities](http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities) they can be specified in the evaporate.yaml as a list under the stack property `capabilities`, e.g.

```
capabilities:
  - CAPABILITY_IAM
```

This field is optional.

#### External Values

Parameter values can refer to environment variables by using the following
syntax:
```
ParamKey: "${env.SOME_ENVIRONMENT_VARIABLE}"
```

Evaporate will fetch the value before creating/updating the stack, throwing an
exception if it cannot be found.

Similarly, stack output values can be used as parameter values as well:
```
ParamKey: ${stack.stack-name-here.output.OutputName}
```

*NB:* The stack whose output value is being used as a parameter value must be
defined in `evaporate.yaml` *before* the stack that requires that value.
Otherwise the stack will be unable to get the output since the other stack won't
exist yet.

At the moment it is not possible to reference an external value as a part of a
larger parameter value, i.e.
```
ParamKey: "Some stuff ${env.SOME_ENV} some other stuff"
```
will not work.

Similarly, multiple external values cannot be referenced within
the same parameter value, i.e.
```
ParamKey: "${env.SOME_ENV}${env.SOME_OTHER_ENV}"
```
will not work.

## Usage

*NB:* Your shell must be authenticated to AWS, e.g., `aws configure`

### Upsert Stack (via binary)

```
> evaporate upsert
```

### Upsert Stack (via Docker)

```
docker run \
  -e AWS_SESSION_TOKEN \
  -e AWS_SECRET_ACCESS_KEY \
  -e AWS_ACCESS_KEY_ID \
  -e AWS_REGION \
  -v $(pwd)/example/stack-template.json:/app/stack-template.json \
  -v $(pwd)/example/evaporate.yaml:/app/evaporate.yaml \
  seek/evaporate upsert
```

Evaporate chooses the parameter values from `evaporate.yaml` based upon
which account you have authenticated to, so you can never accidentally deploy
Sandbox parameters to the Production account.

*NB*: Any files or folders you wish to upload to an S3 bucket must be added
to the docker run command with an extra `-v` flag. You then reference the
locations within your container in `evaporate.yaml`, **NOT** their locations
on your local machine.

## Commands

Evaporate will print detailed usage documentation via the ```--help``` flag.

This works both natively:

```
> evaporate --help
```

and via Docker:
```
> docker run seek/evaporate --help
```


## Troubleshooting

### SignatureDoesNotMatch

Putting your computer to sleep causes a time drift between the docker
container's clock and the host clock. Depending on how long your computer sleeps
for, you may receive a ServiceError similar to this:

```
[ServiceError] {
  service    = STS
  status     = 403 Forbidden
  code       = SignatureDoesNotMatch
  message    = Just Signature expired: 20160810T085646Z is now earlier than 20160811T002228Z (20160811T003728Z - 15 min.)
  request-id = Just c83b97bf-5f5b-11e6-8c56-b7e8c5732999
}
```

If this occurs, restarting docker should fix the problem.
See [here](http://www.awolski.com/failing-aws-cli-on-docker-container-with-old-date-time)
for more information.
