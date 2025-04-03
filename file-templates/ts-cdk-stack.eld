(nil
 '(progn
    (setq type-for-file (string-inflection-pascal-case-function (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))

"import * as cdk from \"aws-cdk-lib\";
import { Construct } from \"constructs\";

type Props = cdk.StackProps;

export class " type-for-file " extends cdk.Stack {
  constructor(scope: Construct, id: string, props: Props) {
    super(scope, id, props);
    " _ "
  }
}"

    '(progn
       (makunbound 'type-for-file)))
