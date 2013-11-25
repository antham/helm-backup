Feature: Replace current buffer with backup content
  In order to replace current buffer with backup
  As a user
  I run helm-backup command

  Background: I created a file and did 3 changes
    Given I open file "/tmp/helm-backup-test/file"
    Then I should be in buffer "file"
    Then I insert "First change" and save
    Given I clear the buffer
    Then I insert "Second change" and save
    Given I clear the buffer
    Then I insert "Third change" and save

  Scenario: I open backup in new buffer
    Given I start an action chain
    And I press "M-x"
    And I type "helm-backup"
    When I press "RET"
    When I press "C-n"
    When I press "C-n"
    When I press "TAB"
    When I press "C-n"
    When I press "RET"
    And I execute the action chain
    Then I should be in buffer matching regexp "/tmp/helm-backup-test/file.*"
    Then I should see "First change"

  Scenario: I replace buffer content with backup
    Given I start an action chain
    And I press "M-x"
    And I type "helm-backup"
    When I press "RET"
    When I press "C-n"
    When I press "C-n"
    When I press "TAB"
    When I press "C-n"
    When I press "C-n"
    When I press "RET"
    And I execute the action chain
