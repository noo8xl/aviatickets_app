package aviatickets.app.actions;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

import aviatickets.app.databaseInit.DatabaseInit;
import aviatickets.app.databaseInit.dto.DatabaseDto;
import aviatickets.app.exception.PermissionDeniedException;
import aviatickets.app.util.HelperService;
import org.springframework.stereotype.Repository;

import aviatickets.app.actions.entity.ActionLog;

@Repository
class ActionRepository {

	private Connection connection = null;
	private Statement statement = null;
	private ResultSet resultSet = null;

	private final DatabaseInit databaseInit;
	private final HelperService helperService;

	ActionRepository(DatabaseInit databaseInit, HelperService helperService){
		this.databaseInit = databaseInit;
		this.helperService = helperService;
	}

  public void saveLog(ActionLog a) throws SQLException, ClassNotFoundException {
    int updated = 0;
		String sql = "INSERT INTO actions (email, action, customer_id) VALUES (?,?,?)";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedStatement = connection.prepareStatement(sql);
			preparedStatement.setString(1, a.email());
			preparedStatement.setString(2, a.action());
			preparedStatement.setInt(3, a.customerId());

			updated += preparedStatement.executeUpdate();
			if (updated != 1) {
				throw new SQLException("Error saving action log");
			}

		}	catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
  }

  public List<ActionLog> getLog(
			Integer skip, Integer limit, Integer customerId, Integer adminId
	) throws SQLException, ClassNotFoundException {

		String isAdmin = "";
    List<ActionLog> logList = new ArrayList<>();
		String sql = "SELECT * FROM actions LIMIT=? OFFSET=? WHERE customer_id=?";
		String isAdminSql = "SELECT role FROM customer_details WHERE customer_id=?";

		try {
			this.initConnection((byte) 0);

			PreparedStatement adminCheck = connection.prepareStatement(isAdminSql);
			adminCheck.setInt(1, customerId);

			this.resultSet = adminCheck.executeQuery();
			while (resultSet.next()) {
				isAdmin = this.resultSet.getString("role");
			}
			if (Boolean.FALSE.equals(isAdmin.equals("ADMIN"))) {
				throw new PermissionDeniedException();
			} else {

				PreparedStatement preparedStatement = connection.prepareStatement(sql);
				preparedStatement.setInt(1, limit);
				preparedStatement.setInt(2, skip);
				preparedStatement.setInt(3, customerId);

				this.resultSet = preparedStatement.executeQuery();
				while (this.resultSet.next()) {
					ActionLog a = this.helperService.getActionEntityFromResultSet(this.resultSet);
					logList.add(a);
				}
			}

		}	catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
		return logList;
  }


	// initConnection -> init database connection before use any repo method
	private void initConnection(Byte type) throws ClassNotFoundException, SQLException {
		DatabaseDto dto = this.databaseInit.initConnection(type);
		this.connection = dto.connection();
		this.statement = dto.statement();
		this.resultSet = dto.resultSet();
	}

	// closeAndStopDBInteraction -> close any active connection before end interaction with each repository method
	private void closeAndStopDBInteraction() throws SQLException {
		DatabaseDto dto = new DatabaseDto(this.connection, this.statement, this.resultSet);
		this.databaseInit.closeAndStopDBInteraction(dto);
	}

}
