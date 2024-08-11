-- drop tables before create a new one:

-- DROP TABLE IF EXISTS ticket;
-- DROP TABLE IF EXISTS ticket_details;
DROP TABLE IF EXISTS customer_two_step_auth;
DROP TABLE IF EXISTS customer_details;
DROP TABLE IF EXISTS actions;

DROP TABLE IF EXISTS purchase_details;
DROP TABLE IF EXISTS purchase;
DROP TABLE IF EXISTS customer;


DROP TABLE IF EXISTS airport_location;
DROP TABLE IF EXISTS airport_contacts;
DROP TABLE IF EXISTS aircraft_features;
DROP TABLE IF EXISTS cabin_class;
DROP TABLE IF EXISTS flights;
DROP TABLE IF EXISTS price_details;
# DROP TABLE IF EXISTS baggage_allowance;
DROP TABLE IF EXISTS aircraft;
DROP TABLE IF EXISTS leg_details;
DROP TABLE IF EXISTS airport;


# REVOKE ALL ON avia_tickets.* FROM testUser@localhost;
DROP USER IF EXISTS testUser@localhost;

DROP VIEW IF EXISTS FULL_FLIGHT_INFO;
DROP VIEW IF EXISTS SHORT_FLIGHT_DATA;

DROP FUNCTION IF EXISTS calculate_test;
DROP FUNCTION IF EXISTS calculate_total_distance;

DROP FUNCTION IF EXISTS get_departure_time_filter;
DROP FUNCTION IF EXISTS calculate_current_price;

DROP PROCEDURE IF EXISTS delete_customer;
DROP PROCEDURE IF EXISTS count_available_sits;
DROP FUNCTION IF EXISTS update_available_sits;


CREATE USER 'testUser'@'localhost' IDENTIFIED BY '*test_Pa$$w0rd%';
GRANT SELECT, INSERT, UPDATE ON avia_tickets.* TO 'testUser'@'localhost';
FLUSH PRIVILEGES;

-- -------------------------------------------------------------------------------------

-- flights area
--

CREATE TABLE IF NOT EXISTS airport (
  id INT NOT NULL AUTO_INCREMENT,
  code varchar(10) NOT NULL,
  airport_name varchar(255) NOT NULL,
  city varchar(50) NOT NULL,
  country varchar(100) NOT NULL,
  terminal varchar(5) NOT NULL,
  timezone varchar(50) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS airport_location (
	id INT NOT NULL AUTO_INCREMENT,
	longitude varchar(30) NOT NULL,
	latitude varchar(30) NOT NULL,
	altitude varchar(30),
	airport_id INT NOT NULL,
	FOREIGN KEY (airport_id) REFERENCES airport (id),
	PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS airport_contacts (
    id INT NOT NULL AUTO_INCREMENT,
    phone varchar(50) NOT NULL,
    email varchar(150) NOT NULL,
    website varchar(255) NOT NULL,
    airport_id INT NOT NULL,
    FOREIGN KEY (airport_id) REFERENCES airport (id),
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS aircraft (
    id INT NOT NULL AUTO_INCREMENT,
    model varchar(50) NOT NULL,
    registration varchar(50) NOT NULL,
    seating_capacity SMALLINT NOT NULL,
    year_of_manufacture SMALLINT NOT NULL,
    PRIMARY KEY (id)
);


CREATE TABLE IF NOT EXISTS aircraft_features (
    id INT NOT NULL AUTO_INCREMENT,
    wifi Boolean NOT NULL,
    entertainment Boolean NOT NULL,
    power_outlets Boolean NOT NULL,
    aircraft_id INT NOT NULL,
    FOREIGN KEY (aircraft_id) REFERENCES aircraft (id),
    PRIMARY KEY (id)
);


CREATE TABLE IF NOT EXISTS cabin_class (
  id INT NOT NULL AUTO_INCREMENT,
  economy Boolean NOT NULL,
  business Boolean NOT NULL,
  first Boolean NOT NULL,
  aircraft_id INT NOT NULL,
  FOREIGN KEY (aircraft_id) REFERENCES aircraft (id),
  PRIMARY KEY (id)
);


CREATE TABLE IF NOT EXISTS price_details (
  id INT NOT NULL AUTO_INCREMENT,
  flight_number varchar(50) NOT NULL,
  currency varchar(5) NOT NULL DEFAULT 'EUR',
  amount FLOAT NOT NULL,
  discount SMALLINT NOT NULL DEFAULT 0,
  baggage varchar(150) NOT NULL,
  PRIMARY KEY (id)
);

-- time format is hh:mm:ss ??*
CREATE TABLE IF NOT EXISTS leg_details (
	id INT NOT NULL AUTO_INCREMENT,

    flight_number varchar(50) NOT NULL,
	departure_airport INT NOT NULL,
	arrival_airport INT NOT NULL,
	departure_time timestamp NOT NULL,
	arrival_time timestamp NOT NULL,
	duration varchar(15) NOT NULL,
	distance SMALLINT NOT NULL,
	status varchar(20) NOT NULL,

	FOREIGN KEY (departure_airport) REFERENCES airport (id),
	FOREIGN KEY (arrival_airport) REFERENCES airport (id),
	PRIMARY KEY (id)
);


CREATE TABLE IF NOT EXISTS flights (
  id INT NOT NULL AUTO_INCREMENT,

  flight_number varchar(50) NOT NULL,
  airline varchar(50) NOT NULL,
  aircraft_id INT NOT NULL,

  distance SMALLINT NOT NULL,
  total_duration varchar(30) NOT NULL,
  price INT NOT NULL,
  passenger_count SMALLINT NOT NULL,
  available_sits SMALLINT NOT NULL,

  FOREIGN KEY (price) REFERENCES price_details (id),
  FOREIGN KEY (aircraft_id) REFERENCES aircraft (id),

  PRIMARY KEY (id)
);

-- -------------------------------------------------------------------------------------

-- customer area
--

CREATE TABLE IF NOT EXISTS customer (
    id INT NOT NULL AUTO_INCREMENT,
    name varchar(30) NOT NULL,
    email varchar(250) NOT NULL,
    password varchar(30) NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS customer_details (
    id INT NOT NULL AUTO_INCREMENT,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
    updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
    is_banned Boolean NOT NULL DEFAULT 0,
    role varchar(10) NOT NULL DEFAULT 'USER',
    customer_id INT NOT NULL,
    FOREIGN KEY (customer_id) REFERENCES customer (id),
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS customer_two_step_auth (
    id INT NOT NULL AUTO_INCREMENT,
    email varchar(250) NOT NULL,
    status BOOLEAN NOT NULL DEFAULT 0,
    expired_at timestamp,
    code varchar(30),
    PRIMARY KEY (id)
);



CREATE TABLE IF NOT EXISTS purchase (
    id INT NOT NULL AUTO_INCREMENT,

    flight_number varchar(50) NOT NULL,
    customer_id INT NOT NULL,
    FOREIGN KEY (customer_id) REFERENCES customer (id),
    PRIMARY KEY (id)
);


 CREATE TABLE IF NOT EXISTS purchase_details (
    id INT NOT NULL AUTO_INCREMENT,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
    updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
    payment_status BOOLEAN NULL DEFAULT 0,
    quantity SMALLINT NOT NULL,
    price FLOAT NOT NULL,
    purchase_id INT NOT NULL,
    FOREIGN KEY (purchase_id) REFERENCES purchase (id),
    PRIMARY KEY (id)
 );


CREATE TABLE IF NOT EXISTS actions (
  id INT NOT NULL AUTO_INCREMENT,
  email varchar(250) NOT NULL,
  date timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  action varchar(250) NOT NULL,
  customer_id INT NOT NULL,
  FOREIGN KEY (customer_id) REFERENCES customer (id),
  PRIMARY KEY (id)
);

-- end of
-- customer area
--

-- -------------------------------------------------------------------------------------


-- get_departure_time_filter -> get range for the hot tickets list view
# CREATE FUNCTION get_departure_time_filter()
#    RETURNS timestamp
#    RETURN CURRENT_TIMESTAMP() + 3600000;


# calculate_total_distance -> get total distance value
# DELIMITER $$
#
# CREATE FUNCTION IF NOT EXISTS calculate_total_distance(
#     flightNum VARCHAR(50)
# )
# RETURNS SMALLINT
# DETERMINISTIC
# BEGIN
#     DECLARE total_distance SMALLINT;
#     DECLARE leg_distance SMALLINT;
#     DECLARE done INT DEFAULT 0;
#     DECLARE cur
#         CURSOR FOR SELECT leg_details.distance
#         FROM leg_details
#         WHERE flight_number=flightNum;
#
#     DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = 1;
#
#     OPEN cur;
#     read_leg: LOOP
#         FETCH cur INTO leg_distance;
#         IF done THEN
#             LEAVE read_leg;
#         END IF;
#         SET total_distance = total_distance + leg_distance;
#     END LOOP;
#
#     CLOSE cur;
#     RETURN total_distance;
# END$$
#
# DELIMITER ;


#
-- calculate_current_price -> calculate value by (price - discount)
# DELIMITER $$
# CREATE FUNCTION IF NOT EXISTS calculate_current_price(
# 	flightNum VARCHAR(50)
# )
# RETURNS FLOAT
# DETERMINISTIC
# BEGIN
#     DECLARE amount FLOAT;
#     DECLARE discount SMALLINT;
#
#     SELECT price_details.amount
#         INTO amount
#         FROM price_details
#         WHERE flight_number=flightNum;
#
#     SELECT price_details.discount
#         INTO discount
#         FROM price_details
#         WHERE flight_number=flightNum;
#
#     return amount - (amount * discount / 100);
# END $$
#
# DELIMITER ;


-- this view collect main flight data to one row
-- by filter like |destination, from-to, airports|
-- and send it as list (skip=20,limit=20) to customer

# CREATE VIEW SHORT_FLIGHT_DATA AS (
#     SELECT
#         flights.id, flights.flight_number, flights.distance=(SELECT calculate_total_distance(flights.flight_number)),
#         price_details.amount=(SELECT calculate_current_price(flights.flight_number)) AS price,
#
#     FROM flights
#     JOIN price_details
#     ON flights.flight_number = price_details.flight_number
# );
#
# #  this view is for collect full flight info
# #  and send it to the customer as <flight details>
#
# CREATE VIEW FULL_FLIGHT_INFO AS (
#     SELECT
#         flights.id, flights.flight_number,
#         price_details.amount=(SELECT calculate_current_price(flights.flight_number)) AS price
#         FROM flights
#         JOIN price_details
#         ON flights.flight_number = price_details.flight_number
# );


#
#
# CREATE PROCEDURE IF NOT EXISTS delete_customer(
#     IN userId INT,
#     IN customerToDeleteId INT
# )
# BEGIN
#     DECLARE userRole VARCHAR(10);
#
#     SELECT customer_details.role
#         INTO userRole
#         FROM customer_details
#         WHERE customer_details.customer_id=userId;
#
#     IF userRole = 'ADMIN' THEN
#         DELETE FROM customer_details WHERE customer_id=customerToDeleteId;
#         DELETE FROM customer_two_step_auth WHERE email=(SELECT email FROM customer WHERE id=customerToDeleteId);
#
#         # delete orders, details, tickets, etc ...
#         # delete <customer> table last
#         DELETE FROM customer WHERE id=customerToDeleteId;
#
#     END IF;
#
#     COMMIT;
# END;

# -- count_available_sits -> count sits by (total sits - sold tickets)

# CREATE FUNCTION IF NOT EXISTS count_available_sits(
#     flightNum VARCHAR(50)
# )
# RETURNS SMALLINT DETERMINISTIC
# BEGIN
#     DECLARE sold_tickets SMALLINT;
#     DECLARE total_sits SMALLINT;
#
#     SELECT COUNT(id)
#         INTO sold_tickets
#         FROM purchase
#         WHERE flight_number=flightNum;
#
#     SELECT flights.passenger_count
#         INTO total_sits
#         FROM flights
#         WHERE flight_number=flightNum;
#
#     return total_sits - sold_tickets;
# END;

#
#
# -- update_available_sits -> update flight available sits after sold tickets check

# CREATE PROCEDURE IF NOT EXISTS update_available_sits (
#     IN flightNum VARCHAR(50)
# )
# BEGIN
#     DECLARE sitsNum INT;
#     SELECT sitsNum=(count_available_sits(flightNum));
#
#     UPDATE flights
#         SET available_sits = sitsNum
#         WHERE flight_number = flightNum;
#
#     COMMIT;
# END;
#
#
#

# DELIMITER $$
# CREATE FUNCTION IF NOT EXISTS get_purchase_details(
#     purchaseId INT
# )
# RETURNS SMALLINT DETERMINISTIC
# BEGIN
#
#     DECLARE sold_tickets SMALLINT;
#     DECLARE total_sits SMALLINT;
#
#     SELECT COUNT(id)
#         INTO sold_tickets
#         FROM purchase
#         WHERE flight_number=flightNum;
#
#     SELECT flights.passenger_count
#         INTO total_sits
#         FROM flights
#         WHERE flight_number=flightNum;
#
#     return total_sits - sold_tickets;
# END $$
# DELIMITER ;



--
--
--
--


# CREATE FUNCTION calculate_test()
#    RETURNS FLOAT DETERMINISTIC
#    RETURN 200.89 - (200.89 * 10 / 100);

-- SELECT calculate_test();