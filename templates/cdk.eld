typescript-ts-base-mode

(stack

"import * as cdk from \"aws-cdk-lib\";
import { Construct } from \"constructs\";

type Props = cdk.StackProps;

export class " (p "NAME") " extends cdk.Stack {
  constructor(scope: Construct, id: string, props: Props) {
    super(scope, id, props);
    " q "
  }" >
"}" >)

(construct

"import * as cdk from \"aws-cdk-lib\";
import { Construct } from \"constructs\";

type Props = {};

export class " (p "NAME") " extends Construct {
  constructor(scope: Construct, id: string, props: Props) {
    super(scope, id);
    " q "
  }" >
"}" >)
