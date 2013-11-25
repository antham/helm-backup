(Given "^I open file \"\\(.+\\)\"$"
  (lambda (filename)
    (find-file filename)
    )
  )

(Then "^I insert \"\\(.+\\)\" and save"
  (lambda (data)
    (insert data)
    (save-buffer)
    )
  )

(Then "^I should be in buffer matching regexp \"\\(.+\\)\"$"
  (lambda (expected)
    (let ((message "Expected to be in buffer '%s', but was in '%s'"))
      (string-match expected (buffer-name)))))


(When "^I run \"\\(.+\\)\"$"
  (lambda (command)
    (execute-extended-command command)
    ))
