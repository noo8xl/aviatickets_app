package aviatickets.app.databaseInit;

import aviatickets.app.databaseInit.dto.DatabaseDto;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

@Component
public class DatabaseInit {

	@Value("${spring.datasource.url}")
	private String dbUrl;

	@Value("${spring.datasource.username}")
	private String adminDbUsername;

	@Value("${spring.datasource.password}")
	private String adminDbPassword;

	@Value("${spring.datasource.admin_name}")
	private String userDbName;

	@Value("${spring.datasource.admin_pwd}")
	private String userDbPassword;

	public DatabaseInit() {}

	// initConnection -> init database connection before use any repo method
	public DatabaseDto initConnection(Byte type) throws ClassNotFoundException, SQLException {
		// type 0 => ADMIN || type 1 => USER
		return type == 0 ? this.initAdminConnection() : initRegularConnection();
	}

	// closeAndStopDBInteraction -> close any active connection before end interaction with each repository method
	public void closeAndStopDBInteraction(DatabaseDto dto) throws SQLException {
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
//			System.out.println("connection is  closed");
		} catch (Exception e) {
			throw e;
		}
	}


	// initAdminConnection -> init database connection AS ADMIN with full access to db
	private DatabaseDto initAdminConnection() throws ClassNotFoundException, SQLException {
		Connection connection = DriverManager.getConnection(this.dbUrl, this.adminDbUsername, this.adminDbPassword);
		return init(connection);
	}

	// initAdminConnection -> init database connection AS USER with limited access to db
	private DatabaseDto initRegularConnection() throws ClassNotFoundException, SQLException {
		Connection connection = DriverManager.getConnection(this.dbUrl, this.userDbName, this.userDbPassword);
		return init(connection);
	}

	private DatabaseDto init(Connection connection) throws SQLException, ClassNotFoundException {
		try {
			Class.forName("com.mysql.cj.jdbc.Driver");
			Statement statement = connection.createStatement();

//			System.out.println("database is connected.");
			return new DatabaseDto(connection, statement, statement.getResultSet());
		} catch (ClassNotFoundException | SQLException e) {
			throw e;
		}
	}
}
