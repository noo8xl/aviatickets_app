package aviatickets.app.customer;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.databaseInit.DatabaseInit;
import aviatickets.app.databaseInit.dto.DatabaseDto;
import aviatickets.app.util.HelperService;
import org.springframework.stereotype.Repository;

import aviatickets.app.customer.entity.Customer;

@Repository
class CustomerRepository {

	private Connection connection = null;
	private Statement statement = null;
	private ResultSet resultSet = null;

	private final DatabaseInit databaseInit;
	private final HelperService helperService;

	CustomerRepository(DatabaseInit databaseInit, HelperService helperService) {
		this.databaseInit = databaseInit;
		this.helperService = helperService;
	}

  public void save(String name, String email, String password) throws SQLException, ClassNotFoundException {

		int savedCustomerId = 0;
		int updated = 0;
    String customerSql = "INSERT INTO customer (name, email, password) VALUES (?,?,?)";
    String detailsSql = "INSERT INTO customer_details (customer_id) VALUES (?)";
		String twoStepParamsSql = "INSERT INTO customer_two_step_auth (email) VALUES(?)";

		try {
			this.initConnection((byte) 0);

			String[] returnedId = {"id"};
			PreparedStatement preparedCustomer = this.connection.prepareStatement(customerSql, returnedId);
			PreparedStatement prepareDetails = this.connection.prepareStatement(detailsSql);
			PreparedStatement prepareTwoStepAuth = this.connection.prepareStatement(twoStepParamsSql);

			preparedCustomer.setString(1, name);
			preparedCustomer.setString(2, email);
			preparedCustomer.setString(3, password);

			updated += preparedCustomer.executeUpdate();

			this.resultSet = preparedCustomer.getGeneratedKeys();
			while (this.resultSet.next()) {
				savedCustomerId = this.resultSet.getInt(1);
			}

			prepareDetails.setInt(1, savedCustomerId);

			prepareTwoStepAuth.setString(1, email);

			updated += prepareDetails.executeUpdate();
			updated += prepareTwoStepAuth.executeUpdate();
			if (updated != 3) {
				throw new SQLException("failed to save customer " + email);
			}
		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
  }

  public List<Customer> findAll(Integer skip, Integer limit) throws SQLException, ClassNotFoundException {

		List<Customer> customersList = new ArrayList<>();
		String sql = "SELECT customer.id, customer.name, customer.email, customer.password, customer_details.created_at, "
				+ "customer_details.role, customer_details.is_banned, customer_two_step_auth.is_enabled as two_step_auth_status "
				+ "FROM customer "
        + "JOIN customer_details ON customer.id = customer_details.customer_id "
				+ "JOIN customer_two_step_auth ON customer.email = customer_two_step_auth.email "
				+ "ORDER BY customer.id"
				+ "LIMIT ?"
				+ "OFFSET ?";

		try {
			this.initConnection((byte) 0);

			PreparedStatement preparedStatement = this.connection.prepareStatement(sql);
			preparedStatement.setInt(1, limit);
			preparedStatement.setInt(2, skip);

			this.resultSet = preparedStatement.executeQuery();
			while (this.resultSet.next()) {
				Customer c = this.helperService.getCustomerEntityFromResultSet(this.resultSet);
				customersList.add(c);
			}
			System.out.println("customersList size is -> " + customersList.size());

		} catch (SQLException e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}

		return customersList;
  }

  public Customer findOne(Integer id) throws SQLException, ClassNotFoundException {

		Customer c = null;
		String sql = "SELECT customer.id, customer.name, customer.email, customer.password, customer_details.created_at, "
				+ "customer_details.role, customer_details.is_banned, customer_two_step_auth.is_enabled as two_step_auth_status "
				+ "FROM customer "
				+ "JOIN customer_details ON customer.id = customer_details.customer_id "
				+ "JOIN customer_two_step_auth ON customer.email = customer_two_step_auth.email "
				+ "WHERE customer.id = ?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedStatement = this.connection.prepareStatement(sql);
			preparedStatement.setInt(1, id);
			this.resultSet = preparedStatement.executeQuery();
			while (this.resultSet.next()) {
				c = this.helperService.getCustomerEntityFromResultSet(this.resultSet);
			}

		} catch (SQLException | ClassNotFoundException e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}

		return c;
  }

  public Customer findOne(String email) throws SQLException, ClassNotFoundException {

		Customer c = null;
		String sql = "SELECT customer.id, customer.name, customer.email, customer.password, customer_details.created_at, "
				+ "customer_details.role, customer_details.is_banned, customer_two_step_auth.is_enabled as two_step_auth_status "
				+ "FROM customer "
				+ "JOIN customer_details ON customer.id=customer_details.customer_id "
				+ "JOIN customer_two_step_auth ON customer.email=customer_two_step_auth.email "
				+ "WHERE customer.email = ?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedStatement = this.connection.prepareStatement(sql);
			preparedStatement.setString(1, email);

			this.resultSet = preparedStatement.executeQuery();
			while (this.resultSet.next()) {
				c = this.helperService.getCustomerEntityFromResultSet(this.resultSet);
			}

		} catch (SQLException | ClassNotFoundException e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}

		return c;
  }


  public void update(Customer c) throws SQLException, ClassNotFoundException {

		int updated = 0;
    String sqlBase = "UPDATE customer SET name=?, password=? WHERE id=?";
    String sqlParams = "UPDATE customer_details SET updated_at=?, is_banned=?, role=? WHERE customer_id=?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedBase = this.connection.prepareStatement(sqlBase);
			PreparedStatement preparedParams = this.connection.prepareStatement(sqlParams);

			preparedBase.setString(1, c.name());
			preparedBase.setString(2, c.password());
			preparedBase.setInt(3, c.id());

			preparedParams.setDate(1, new Date(System.currentTimeMillis()));
			preparedParams.setBoolean(2, c.isBanned());
			preparedParams.setString(3, c.role());
			preparedParams.setInt(4, c.id());

			updated += preparedBase.executeUpdate();
			updated += preparedParams.executeUpdate();

			if (updated != 2) {
				throw new SQLException("failed to update customer " + c.name());
			}

		} catch (SQLException | ClassNotFoundException e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
  }

  public void delete(Integer idToDelete, Integer adminId) throws SQLException, ClassNotFoundException {
    // delete each record in each table by userId *

		String sql = String.format("CALL delete_customer(%d, %d)", adminId, idToDelete);
		try {
			// only ADMIN db user has access to delete method *
			this.initConnection((byte) 0);

			PreparedStatement preparedStatement = this.connection.prepareStatement(sql);
			int updated = preparedStatement.executeUpdate();
			if (updated < 1) {
				throw new SQLException("Failed to delete customer " + idToDelete);
			}
		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
  }

	public Boolean getTwoStepStatus(String email) throws SQLException, ClassNotFoundException {

		boolean isEnabled = false;
		String sql = "SELECT customer_two_step_auth.is_enabled FROM customer_two_step_auth WHERE email=?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedStatus = this.connection.prepareStatement(sql);
			preparedStatus.setString(1, email);

			this.resultSet = preparedStatus.executeQuery();
			while (this.resultSet.next()) {
				isEnabled = this.resultSet.getBoolean("is_enabled");
			}
		} catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
		return isEnabled;
	}

	public void update2faStatus(ChangeTwoStepStatusDto dto) throws SQLException, ClassNotFoundException {

		int updated = 0;
		String sql = "UPDATE customer_two_step_auth SET is_enabled=? WHERE email=?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedStatus = this.connection.prepareStatement(sql);
			preparedStatus.setBoolean(1, dto.status());
			preparedStatus.setString(2, dto.email());

			updated += preparedStatus.executeUpdate();
			if (updated < 1) {
				throw new SQLException("Failed to update customer " + dto.email());
			}
		}	catch (Exception e) {
			throw e;
		} finally {
			this.closeAndStopDBInteraction();
		}
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
