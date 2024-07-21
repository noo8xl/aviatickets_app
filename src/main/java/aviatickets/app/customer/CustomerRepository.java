package aviatickets.app.customer;

import java.sql.*;
import java.util.List;
import java.util.Optional;

import aviatickets.app.databaseInit.DatabaseInit;
import aviatickets.app.databaseInit.dto.DatabaseDto;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;

import aviatickets.app.customer.entity.Customer;

@Repository
public class CustomerRepository {

	private Connection connection = null;
	private Statement statement = null;
	private ResultSet resultSet = null;

	private final DatabaseInit databaseInit;
	private JdbcClient jdbcClient;

	public CustomerRepository(DatabaseInit databaseInit, JdbcClient jdbcClient) {
		this.databaseInit = databaseInit;
		this.jdbcClient = jdbcClient;
	}

  public void save(Customer customer) {

    String userBase = "INSERT INTO customer (name, email, password) VALUES (?,?,?)";
    String userParams = "INSERT INTO customer_details (created_at, updated_at, is_banned, role, customer_id) VALUES (?,?,?,?,?)";
    // base user data



    var updated = jdbcClient.sql(userBase)
        .params(List.of(customer.name(), customer.email(), customer.password()))
        .update();

    Assert.state(updated == 1, "Failed to create user " + customer.name());

    // get saved user
    Optional<Customer> savedCusromer = this.findByEmail(customer.email());
    var customerId = 0;
    if (savedCusromer.isPresent())
      customerId = savedCusromer.get().id();
    // user params data
    updated += jdbcClient.sql(userParams)
        .params(List.of(customer.createdAt(), customer.updatedAt(), customer.isBanned(), customer.role(),
            customerId))
        .update();

    Assert.state(updated == 2, "Failed to create user " + customer.name());
  }

  public List<Customer> findAll() {
    String sqlStr = "SELECT customer.id, customer.name, customer.email, customer_details.created_at, customer_details.updated_at, customer_details.role "
        + "FROM customer "
        + "INNER JOIN customer_details ON customer.id = customer_details.customer_id ";

    return jdbcClient.sql(sqlStr)
        .query(Customer.class)
        .list();
  }

  @Cacheable("userList")
  public Optional<Customer> findById(Integer id) {

    String sqlStr = "SELECT customer.id, customer.name, customer.email, customer_details.created_at, customer_details.updated_at, customer_details.role "
        + "FROM customer "
        + "INNER JOIN customer_details ON customer.id = customer_details.customer_id "
        + "WHERE customer.id=?";

    return jdbcClient.sql(sqlStr)
        .param(id)
        .query(Customer.class)
        .optional();
  }

  public Optional<Customer> findByEmail(String email) {

    String sqlStr = "SELECT customer.id, customer.name, customer.email, customer_details.created_at, customer_details.updated_at, customer_details.role "
        + "FROM customer "
        + "INNER JOIN customer_details ON customer.id = customer_details.customer_id "
        + "WHERE customer.email=?";

    return jdbcClient.sql(sqlStr)
        .param(email)
        .query(Customer.class)
        .optional();
  }

  public void update(Customer c, Integer id) {
    String updateBase = "UPDATE customer SET name=?, email=?, password=? WHERE id=?";
    String updateParams = "UPDATE customer_details SET updated_at=?, is_banned=?, role=? WHERE customer_id=?";

    var updated = jdbcClient.sql(updateBase)
        .params(List.of(c.name(), c.email(), c.password(), id))
        .update();

    Assert.state(updated == 1, "Failed to update customer " + c.name());

    updated += jdbcClient.sql(updateParams)
        .params(List.of(c.updatedAt(), c.isBanned(), c.role(), c.id()))
        .update();

    Assert.state(updated == 2, "Failed to update customer params " + c.name());

  }

  public void delete(Integer idToDelete, Integer customerId) throws SQLException, ClassNotFoundException {
    // delete each record in each table by userId *

		String sql = String.format("CALL delete_customer(%d, %d)", customerId, idToDelete);
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
