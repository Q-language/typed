# Argument types

    Code
      writeLines(rpp_elide("?function(a = 0L) {}"))
    Output
      function(a = 0L) {} # !q ?function(a = 0L) {}

---

    Code
      writeLines(rpp_elide("?function(a = ?Integer()) {}"))
    Output
      function(a             ) {} # !q ?function(a = ?Integer()) {}

---

    Code
      writeLines(rpp_elide("?function(a = 0L ?Integer()) {}"))
    Output
      function(a = 0L) {} # !q ?function(a = 0L ?Integer()) {}

# Return values

    Code
      writeLines(rpp_elide("Integer()? function(a = 0L ?Integer()) {}"))
    Output
      function(a = 0L) {} # !q Integer()? function(a = 0L ?Integer()) {}

---

    Code
      writeLines(rpp_elide("fun <- (Integer()? function(a = 0L ?Integer()) {})"))
    Output
      fun <- (function(a = 0L) {}) # !q fun <- (Integer()? function(a = 0L ?Integer()) {})

---

    Code
      writeLines(rpp_elide("fun <- Integer()? function(a = 0L ?Integer()) {}"))
    Output
      fun <-            function(a = 0L) {} # !q fun <- Integer()? function(a = 0L ?Integer()) {}

---

    Code
      writeLines(rpp_elide(code))
    Output
      fun <-            function(a = 0L,   # !q fun <- Integer()? function(a = 0L ?Integer(),
                                 b = '') { # !q                            b = '' ?Character()) {
      }

# Variable types

    Code
      writeLines(rpp_elide("Integer()? x <- 0L"))
    Output
      x <- 0L # !q Integer()? x <- 0L

