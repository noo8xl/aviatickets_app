DROP TABLE IF EXISTS user;
DROP TABLE IF EXISTS user_details;
DROP TABLE IF EXISTS ticket;
DROP TABLE IF EXISTS ticket_details;
DROP TABLE IF EXISTS actions;



CREATE TABLE IF NOT EXISTS user (
  id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  name varchar(30) NOT NULL,
  email varchar(250) NOT NULL,
  password varchar(30) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS user_details (
  id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL,
  role varchar(10) NOT NULL,
  user_id INT UNSIGNED NOT NULL,
  FOREIGN KEY (user_id = user.id),
  PRIMARY KEY (id)
);

-- CREATE TABLE IF NOT EXISTS user_orders (
--   id INT UNSIGNED NOT NULL AUTO_INCREMENT,
--   created_at timestamp NOT NULL,
--   updated_at timestamp NOT NULL,
--   role Boolean NOT NULL,
--   user_id INT UNSIGNED NOT NULL,
--   FOREIGN KEY (user_id = user.id),
--   PRIMARY KEY (id)
-- );

CREATE TABLE IF NOT EXISTS ticket (
  id INT UNSIGNED NOT NULL AUTO_INCREMENT,


  user_id INT UNSIGNED NOT NULL,
  FOREIGN KEY (user_id = user.id),
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS ticket_details (
  id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL,
  role Boolean NOT NULL,
  ticket_id INT UNSIGNED NOT NULL,
  FOREIGN KEY (user_id = ticket.id),
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS actions (
  id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  email varchar(250) NOT NULL,
  action_date timestamp NOT NULL,
  action varchar(250) NOT NULL,
  user_id INT UNSIGNED NOT NULL,
  FOREIGN KEY (user_id = user.id),
  PRIMARY KEY (id)
);