package aviatickets.app.actions;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

import aviatickets.app.databaseInit.DatabaseInit;
import aviatickets.app.databaseInit.dto.DatabaseDto;
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
			preparedStatement.setString(1, a.getCustomerEmail());
			preparedStatement.setString(2, a.getCustomerAction());
			preparedStatement.setInt(3, a.getCustomerId());

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

  public List<ActionLog> getLog(Integer skip, Integer limit, Integer customerId) throws SQLException, ClassNotFoundException {

		ActionLog a = null;
    List<ActionLog> logList = new ArrayList<>();

		String sql = "SELECT * FROM actions WHERE customer_id=? ORDER BY id LIMIT ? OFFSET ?";

		try {
			this.initConnection((byte) 0);

			PreparedStatement preparedStatement = this.connection.prepareStatement(sql);
			preparedStatement.setInt(1, customerId);
			preparedStatement.setInt(2, limit);
			preparedStatement.setInt(3, skip);

			this.resultSet = preparedStatement.executeQuery();
			while (this.resultSet.next()) {
				a = this.helperService.getActionEntityFromResultSet(this.resultSet);
				logList.add(a);
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
