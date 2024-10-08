package aviatickets.app.customer;

import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.customer.dto.UpdateCustomerDto;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.database.DatabaseInterface;
import aviatickets.app.database.dto.DBConnectionDto;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.exception.NotFoundException;
import aviatickets.app.util.SerializationInterface;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

@RequiredArgsConstructor
@Repository
class CustomerRepository implements CustomerInterface {

	private static final Logger log = LoggerFactory.getLogger(CustomerRepository.class);

	private Connection connection = null;
	private Statement statement = null;
	private ResultSet resultSet = null;

	private final DatabaseInterface database;
	private final SerializationInterface serializationService;

	@Override
  public void save(String name, String email, String password) throws SQLException, ClassNotFoundException {

		int savedCustomerId = 0;
		int updated = 0;
		Customer c;

    String customerSql = "INSERT INTO customer (name, email, password) VALUES (?,?,?)";
    String detailsSql = "INSERT INTO customer_details (customer_id) VALUES (?)";
		String twoStepParamsSql = "INSERT INTO customer_two_step_auth (email) VALUES(?)";

		try {
			String[] returnedId = {"id"};
			c = this.findOne(email);
			if (Boolean.TRUE.equals((c != null))) {
				throw new BadRequestException("Bad request. Email has been already taken.");
			}

			this.initConnection((byte) 0);

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
		} finally {
			this.closeAndStopDBInteraction();
		}
  }

	@Override
  public List<Customer> findAll(Integer skip, Integer limit) throws SQLException, ClassNotFoundException {

		List<Customer> customersList = new ArrayList<>();
		String sql = "SELECT customer.id, customer.name, customer.email, customer.password, customer_details.created_at, "
				+ "customer_details.role, customer_details.is_banned, customer_two_step_auth.status as two_step_auth_status "
				+ "FROM customer "
        + "JOIN customer_details ON customer.id = customer_details.customer_id "
				+ "JOIN customer_two_step_auth ON customer.email = customer_two_step_auth.email "
				+ "ORDER BY customer.id "
				+ "LIMIT ? "
				+ "OFFSET ?";

		try {
			this.initConnection((byte) 0);

			PreparedStatement preparedStatement = this.connection.prepareStatement(sql);
			preparedStatement.setInt(1, limit);
			preparedStatement.setInt(2, skip);

			this.resultSet = preparedStatement.executeQuery();
			while (this.resultSet.next()) {
				Customer c = this.serializationService.getCustomerEntityFromResultSet(this.resultSet);
				customersList.add(c);
			}

		} finally {
			this.closeAndStopDBInteraction();
		}

		return customersList;
  }

@Override
  public Customer findOne(Integer id) throws SQLException, ClassNotFoundException {
		Customer c = null;
		String sql = "SELECT customer.id, customer.name, customer.email, customer.password, "
				+ "customer_details.is_banned, customer_details.role, customer_two_step_auth.status as two_step_auth_status "
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
				c = this.serializationService.getCustomerEntityFromResultSet(this.resultSet);
			}

		} finally {
			this.closeAndStopDBInteraction();
		}

		return c;
  }

@Override
  public Customer findOne(String email) throws SQLException, ClassNotFoundException {
		Customer c = null;
		String sql = "SELECT customer.id, customer.name, customer.email, customer.password, "
				+ "customer_details.is_banned, customer_details.role, customer_two_step_auth.status as two_step_auth_status "
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
				c = this.serializationService.getCustomerEntityFromResultSet(this.resultSet);
			}

		} finally {
			this.closeAndStopDBInteraction();
		}

		return c;
  }


	@Override
  public void updateProfile(UpdateCustomerDto dto) throws SQLException, ClassNotFoundException {

		int updated = 0;
    String sqlBase = "UPDATE customer SET name=?, password=? WHERE id=?";
    String sqlParams = "UPDATE customer_details SET updated_at=? WHERE customer_id=?";

		try {
			this.isCustomerExists(dto.email());
			this.initConnection((byte) 1);


			PreparedStatement preparedBase = this.connection.prepareStatement(sqlBase);
			PreparedStatement preparedParams = this.connection.prepareStatement(sqlParams);

			preparedBase.setString(1, dto.name());
			preparedBase.setString(2, dto.password());
			preparedBase.setInt(3, dto.id());

			preparedParams.setDate(1, new Date(System.currentTimeMillis()));
			preparedParams.setInt(2, dto.id());

			updated += preparedBase.executeUpdate();
			updated += preparedParams.executeUpdate();

			if (updated != 2) {
				throw new SQLException("failed to update customer " + dto.name());
			}

		} finally {
			this.closeAndStopDBInteraction();
		}
  }

	// should be updated -<<<

	@Override
	public void updatePassword(String email, String pwd) throws SQLException, ClassNotFoundException {

		int customerId;
		int updated = 0;
		String customerSql = "UPDATE customer SET password=? WHERE email=?";
		String detailsSql = "UPDATE customer_details SET updated_at=? WHERE customer_id=?";

		try {
			this.isCustomerExists(email);
			this.initConnection((byte) 1);


			PreparedStatement preparedCustomer = this.connection.prepareStatement(customerSql);
			PreparedStatement preparedDetails = this.connection.prepareStatement(detailsSql);

			preparedCustomer.setString(1, pwd);
			preparedCustomer.setString(2, email);

			updated += preparedCustomer.executeUpdate();
			customerId = this.findOne(email).getId();

			preparedDetails.setDate(1, new Date(System.currentTimeMillis()));
			preparedDetails.setInt(2, customerId);

			updated += preparedDetails.executeUpdate();

			if (updated != 2) {
				throw new SQLException("failed to update customer " + email);
			}

		} finally {
			this.closeAndStopDBInteraction();
		}
	}

	@Override
  public void deleteCustomer(Integer idToDelete, Integer adminId) throws SQLException, ClassNotFoundException {

		String sql = "CALL delete_customer(?,?)";

		try {
			this.initConnection((byte) 0);

			PreparedStatement statement = this.connection.prepareStatement(sql);
			statement.setInt(1, idToDelete);
			statement.setInt(2, adminId);

			statement.execute();
			log.info("updated count -> {}", statement.getUpdateCount());

			if (statement.getUpdateCount() < 1) {
				throw new SQLException("Failed to delete customer " + idToDelete);
			}
		} finally {
			this.closeAndStopDBInteraction();
		}
  }

@Override
public Boolean getTwoStepStatus(String email) throws SQLException, ClassNotFoundException {

		boolean status = false;
		String sql = "SELECT status FROM customer_two_step_auth WHERE email=?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedStatus = this.connection.prepareStatement(sql);
			preparedStatus.setString(1, email);

			this.resultSet = preparedStatus.executeQuery();
			while (this.resultSet.next()) {
				status = this.resultSet.getBoolean("status");
			}
		} finally {
			this.closeAndStopDBInteraction();
		}
		return status;
	}

	@Override
	public void update2faStatus(ChangeTwoStepStatusDto dto) throws SQLException, ClassNotFoundException {

		int updated = 0;
		String sql = "UPDATE customer_two_step_auth SET status=?, type=?, telegramId=? WHERE email=?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedStatus = this.connection.prepareStatement(sql);
			preparedStatus.setBoolean(1, dto.status());
			preparedStatus.setString(2, dto.type());
			preparedStatus.setString(3, dto.telegramId());
			preparedStatus.setString(4, dto.email());

			updated += preparedStatus.executeUpdate();
			if (updated < 1) {
				throw new SQLException("Failed to update customer " + dto.email());
			}
		} finally {
			this.closeAndStopDBInteraction();
		}
	}

	@Override
	public String getTwoStepStatusType(String email) throws SQLException, ClassNotFoundException {

		String type = "";
		String sql = "SELECT type FROM customer_two_step_auth WHERE email=?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedStatus = this.connection.prepareStatement(sql);
			preparedStatus.setString(1, email);

			this.resultSet = preparedStatus.executeQuery();
			while (this.resultSet.next()) {
				type = this.resultSet.getString("type");
			}
		} finally {
			this.closeAndStopDBInteraction();
		}
		return type;
	}

	@Override
	public String getCustomerTelegramId(String email) throws SQLException, ClassNotFoundException {

		String telegramId = "";
		String sql = "SELECT telegramId FROM customer_two_step_auth WHERE email=?";

		try {
			this.initConnection((byte) 1);

			PreparedStatement preparedStatus = this.connection.prepareStatement(sql);
			preparedStatus.setString(1, email);

			this.resultSet = preparedStatus.executeQuery();
			while (this.resultSet.next()) {
				telegramId = this.resultSet.getString("telegramId");
			}
		} finally {
			this.closeAndStopDBInteraction();
		}
		return telegramId;
	}

@Override
	public void updateBanStatus(Integer id, Boolean status) throws SQLException, ClassNotFoundException {

		int updated = 0;
		String sql = "UPDATE customer_details SET is_banned=? WHERE customer_id=?";

		try {
			this.initConnection((byte) 0);

			PreparedStatement preparedStatus = this.connection.prepareStatement(sql);
			preparedStatus.setInt(1, id);
			preparedStatus.setBoolean(2, status);

			updated += preparedStatus.executeUpdate();
			if (updated < 1) {
				throw new SQLException("Failed to update customer " + id);
			}
		} finally {
			this.closeAndStopDBInteraction();
		}
	}

	// #############################################################################################################

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

	// used only a few times in update methods to check is customer exists before update something data
	private void isCustomerExists(String email) throws SQLException, ClassNotFoundException {
		Customer c = this.findOne(email);
		if (Boolean.FALSE.equals((c != null))) {
			throw new NotFoundException();
		}
	}


}
