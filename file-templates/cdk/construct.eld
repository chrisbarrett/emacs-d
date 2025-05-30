(nil
 '(progn
    (setq type-for-file (string-inflection-pascal-case-function
                         (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))

 "import { Construct } from \"constructs\";

type Props = {
};

export class " type-for-file " extends Construct {
  constructor(scope: Construct, id: string, props: Props) {
    super(scope, id);
    " _ "
  }
}"

 '(progn
    (makunbound 'type-for-file)))
