package aviatickets.app.purchase;

import aviatickets.app.databaseInit.DatabaseInit;
import aviatickets.app.databaseInit.dto.DatabaseDto;
import aviatickets.app.purchase.dto.request.CreatePurchaseDto;
import aviatickets.app.purchase.dto.request.UpdatePurchaseDto;
import aviatickets.app.purchase.entity.Purchase;
import aviatickets.app.util.HelperService;
import org.springframework.stereotype.Repository;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

@Repository
class PurchaseRepository implements PurchaseInteraction {

		private Connection connection = null;
		private Statement statement = null;
		private ResultSet resultSet = null;

		private final DatabaseInit databaseInit;
		private final HelperService helperService;

	PurchaseRepository(DatabaseInit databaseInit, HelperService helperService) {
		this.databaseInit = databaseInit;
		this.helperService = helperService;
	}

	@Override
	public void create(CreatePurchaseDto dto) throws SQLException, ClassNotFoundException {

		int savedId = 0;
		int updated = 0;

		String baseSql = "INSERT INTO purchase (flight_number, customer_id) VALUES (?,?)";
		String detailSql = "INSERT INTO purchase_details (price, purchase_id) VALUES (?,?)";

		try {
			this.initConnection((byte) 1);

			String[] returnedId = {"id"};
			PreparedStatement preparedBase = this.connection.prepareStatement(baseSql, returnedId);
			PreparedStatement preparedDetail = this.connection.prepareStatement(detailSql);

			preparedBase.setString(1, dto.flightNumber());
			preparedBase.setInt(2, dto.customerId());

			updated += preparedBase.executeUpdate();

			this.resultSet = preparedBase.getGeneratedKeys();
			while (this.resultSet.next()) {
				savedId = this.resultSet.getInt(1);
			}

			preparedDetail.setFloat(1, dto.price());
			preparedDetail.setInt(2, savedId);

			updated += preparedDetail.executeUpdate();

			if (updated != 3) {
				throw new SQLException("failed to save purchase");
			}

		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
	}

	@Override
	public void confirmPurchase(Integer purchaseId) throws SQLException, ClassNotFoundException {

		int updated = 0;
		// -> should be updated
		String sql = "UPDATE purchase_details "
				+ "SET purchase_details.payment_status = ?, purchase_details.updated_at = ? "
				+ "WHERE purchase_id = ? ";

		try {
			this.initConnection((byte) 1);

			PreparedStatement statement = this.connection.prepareStatement(sql);
			statement.setBoolean(1, true);
			statement.setDate(2, new Date(System.currentTimeMillis()) );
			statement.setInt(3, purchaseId);

			updated += statement.executeUpdate();

			if (updated < 1) {
				throw new SQLException("failed to confirm purchase");
			}

		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
	}

	@Override
	public Purchase getDetails(Integer id) throws SQLException, ClassNotFoundException {

		Purchase p = new Purchase();

		// -> should be updated
		String sql = "SELECT purchase.flight_number, purchase_details.created_at, purchase_details.payment_status, purchase.id "
				+ "FROM purchase "
				+ "INNER JOIN purchase_details "
				+ "ON purchase.id = purchase_details.purchase_id "
				+ "WHERE purchase.id = ? ";

		try {
			this.initConnection((byte) 1);

			PreparedStatement statement = this.connection.prepareStatement(sql);
			statement.setInt(1, id);

			this.resultSet = statement.executeQuery();
			while (this.resultSet.next()) {
				p = this.helperService.getPurchaseEntityFromResultSet(this.resultSet);
			}

		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}

		return p;
	}


	@Override
	public List<Purchase> getHistory(Integer customerId, Short skip, Short limit) throws SQLException, ClassNotFoundException {

		List<Purchase> history = new ArrayList<>();
		// -> should be updated
		String sql = "SELECT purchase.flight_number, purchase_details.created_at, purchase_details.payment_status, purchase.id "
				+ "FROM purchase "
				+ "INNER JOIN purchase_details "
				+ "ON purchase.id = purchase_details.purchase_id "
				+ "WHERE purchase.id = ? "
				+ "ORDER BY created_at "
				+ "DESC "
				+ "LIMIT ? "
				+ "OFFSET ?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement statement = this.connection.prepareStatement(sql);

			statement.setInt(1, customerId);
			statement.setInt(2, limit);
			statement.setInt(3, skip);

			this.resultSet = statement.executeQuery();
			while (this.resultSet.next()) {
				Purchase p = this.helperService.getPurchaseEntityFromResultSet(this.resultSet);
				history.add(p);
			}

		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}

		return history;
	}


// ##########################################################################################################
// ##################################### ADMIN permission only ##############################################
// ##########################################################################################################


	@Override
	public List<Purchase> getAll(Short skip, Short limit) throws SQLException, ClassNotFoundException {

		List<Purchase> purchaseList = new ArrayList<>();
		// -> should be updated
		String sql = "SELECT purchase.flight_number, purchase_details.created_at, purchase.id "
				+ "FROM purchase "
				+ "INNER JOIN purchase_details "
				+ "ON purchase.id = purchase_details.purchase_id "
				+ "ORDER BY purchase_details.created_at "
				+ "DESC "
				+ "LIMIT ? "
				+ "OFFSET ?";

		try {
			this.initConnection((byte) 0);

			PreparedStatement statement = this.connection.prepareStatement(sql);
			statement.setInt(1, limit);
			statement.setInt(2, skip);

			this.resultSet = statement.executeQuery();
			while (this.resultSet.next()) {
				Purchase p = this.helperService.getPurchaseEntityFromResultSet(this.resultSet);
				purchaseList.add(p);
			}

		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}

		return purchaseList;
	}

	@Override
	public void update(UpdatePurchaseDto dto) throws SQLException, ClassNotFoundException {

		int updated = 0;

		String baseSql = "UPDATE purchase SET flight_number = ? WHERE id = ?";
		String detailSql = "UPDATE purchase_details "
				+ "SET price = ?, payment_status = ?, quantity = ? updated_at = ? "
				+ "WHERE purchase_id = ?";

		try {
			this.initConnection((byte) 0);

			PreparedStatement baseStatement = this.connection.prepareStatement(baseSql);
			PreparedStatement detailStatement = this.connection.prepareStatement(detailSql);

			baseStatement.setString(1, dto.flightNumber());
			baseStatement.setInt(2, dto.id());

			detailStatement.setFloat(1, dto.price());
			detailStatement.setBoolean(2, dto.paymentStatus());
			detailStatement.setShort(3, dto.quantity());
			detailStatement.setInt(4, dto.id());

			updated += baseStatement.executeUpdate();
			updated += detailStatement.executeUpdate();

			if(updated < 2) {
				throw new SQLException("failed to update purchase");
			}

		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
	}

	@Override
	public void delete(Integer id) throws SQLException, ClassNotFoundException {

		try {
			this.initConnection((byte) 0);


		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
	}

// #############################################################################################################


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