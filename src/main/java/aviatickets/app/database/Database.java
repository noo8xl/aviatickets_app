package aviatickets.app.database;

import aviatickets.app.database.dto.DBConnectionDto;
import lombok.NoArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.sql.*;

@NoArgsConstructor
@Component
public class Database implements DatabaseInterface {

	@Value("${spring.datasource.url}")
	private String dbUrl;

	@Value("${spring.datasource.username}")
	private String adminDbUsername;

	@Value("${spring.datasource.password}")
	private String adminDbPassword;

	@Value("${spring.datasource.dbUserName}")
	private String userDbName;

	@Value("${spring.datasource.dbUserPwd}")
	private String userDbPassword;

	private final Logger log = LoggerFactory.getLogger(Database.class);

	@Override
	public DBConnectionDto initConnection(Byte type) throws ClassNotFoundException, SQLException {
		// type 0 => ADMIN || type 1 => USER
		return type == 0 ? this.initAdminConnection() : this.initRegularConnection();
	}

	@Override
	public void closeAndStopDBInteraction(DBConnectionDto dto) throws SQLException {
		try {
			if (dto.resultSet() != null) {
				dto.resultSet().close();
			}

			if (dto.statement() != null) {
				dto.statement().close();
			}

			if (dto.connection() != null) {
				dto.connection().close();
			}
			log.info("connection is closed");
		} catch (Exception e) {
			log.info(e.getMessage());
			throw e;
		}
	}

	// ##########################################################################################################

	private DBConnectionDto initAdminConnection() throws ClassNotFoundException, SQLException {
		Connection connection;
		try {
			connection = DriverManager.getConnection(this.dbUrl, this.adminDbUsername, this.adminDbPassword);
		} catch (SQLException e) {
			log.info(e.getMessage());
			throw e;
		}

		return init(connection);
	}

	private DBConnectionDto initRegularConnection() throws ClassNotFoundException, SQLException {
		Connection connection;
		try {
			connection = DriverManager.getConnection(this.dbUrl, this.userDbName, this.userDbPassword);
		} catch (SQLException e) {
			log.info(e.getMessage());
			throw e;
		}

		return init(connection);
	}

	private DBConnectionDto init(Connection connection) throws SQLException, ClassNotFoundException {
		try {
			Class.forName("com.mysql.cj.jdbc.Driver");
			Statement statement = connection.createStatement();

			log.info("database is connected.");
			return new DBConnectionDto(connection, statement, statement.getResultSet());
		} catch (ClassNotFoundException | SQLException e) {
			log.info(e.getMessage());
			throw e;
		}
	}
}
