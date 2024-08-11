package aviatickets.app.actions;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

import aviatickets.app.database.DatabaseInterface;
import aviatickets.app.database.dto.DBConnectionDto;
import aviatickets.app.util.HelperInterface;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import aviatickets.app.actions.entity.ActionLog;

@RequiredArgsConstructor
@Repository
class ActionRepository implements ActionInterface {

	private Connection connection = null;
	private Statement statement = null;
	private ResultSet resultSet = null;

	private final DatabaseInterface database;
	private final HelperInterface helperService;

	@Override
  public void saveLog(ActionLog a) throws SQLException, ClassNotFoundException {

    int updated = 0;
		String sql = "INSERT INTO actions (email, action, customer_id) VALUES (?,?,?)";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedStatement = this.connection.prepareStatement(sql);
			preparedStatement.setString(1, a.getEmail());
			preparedStatement.setString(2, a.getAction());
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

	@Override
  public List<ActionLog> getLog(Integer skip, Integer limit, Integer customerId) throws SQLException, ClassNotFoundException {

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
				ActionLog a = this.helperService.getActionEntityFromResultSet(this.resultSet);
				logList.add(a);
			}

		}	catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
		return logList;
  }

// ###########################################################

	private void initConnection(Byte type) throws ClassNotFoundException, SQLException {
		DBConnectionDto dto = this.database.initConnection(type);
		this.connection = dto.connection();
		this.statement = dto.statement();
		this.resultSet = dto.resultSet();
	}

	private void closeAndStopDBInteraction() throws SQLException, ClassNotFoundException {
		DBConnectionDto dto = new DBConnectionDto(this.connection, this.statement, this.resultSet);
		this.database.closeAndStopDBInteraction(dto);
	}

}
