package aviatickets.app.database;

import aviatickets.app.database.dto.DBConnectionDto;

import java.sql.Connection;
import java.sql.SQLException;

// AbstractDatabase -> describe internal db.class methods
abstract class AbstractDatabase {
// init db connection as ADMIN user
abstract protected DBConnectionDto initAdminConnection() throws ClassNotFoundException, SQLException;

// init db connection as regular user with limited rights
abstract protected DBConnectionDto initRegularConnection() throws ClassNotFoundException, SQLException;

// set DBConnectionDto entity
abstract protected DBConnectionDto init(Connection connection) throws SQLException, ClassNotFoundException;

}
