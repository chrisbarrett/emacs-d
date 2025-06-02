terraform-mode

(o "output \"" (p "NAME") "\" {" n> "value = " q n "}" >)
(v "variable \"" (p "NAME") "\" {" n> q n "}" >)
(r "resource \"" (p "TYPE") "\" \"" (p "NAME") "\" {" n> q n "}" >)
(d "data \"" (p "TYPE") "\" \"" (p "NAME") "\" {" n> q n "}" >)

;; IAM policies

(pol "\
data \"aws_iam_policy_document\" \"" (p "ID" id) "\" {
  statement {
    sid = \"" q "\"
    effect = \"ALLOW\"
    actions = []
    resources = []
  }
}

resource \"aws_iam_policy\" \"" id "\" {
  name        = \"spacelift-execution-policy\"
  description = \"\"
  policy      = data.aws_iam_policy_document." id ".json
}
")

(st "\
statement {
    sid = \"" q "\"
    effect = \"ALLOW\"
    actions = []
    resources = []
}" >)

(role "\
resource \"aws_iam_role\" \"" (p "ID" role) "\" {
  name               = \"" role "\"
  description        = \"\"
  assume_role_policy = data.aws_iam_policy_document." role ".json
}

data \"aws_iam_policy_document\" \"" role "\" {
  statement {
    sid = \"AssumeRole\"
    effect = \"ALLOW\"
    actions = [\"sts:AssumeRole\"]

    condition {
      test = \"\"
      variable = \"\"
      values = []
    }

    principals {

    }
  }
}
")

(att "\
resource \"aws_iam_role_policy_attachment\" \"" (p "ID") "\" {
  role       = aws_iam_role." (p "ROLE") ".name
  policy_arn = aws_iam_policy." (p "POLICY") ".arn
}
")
