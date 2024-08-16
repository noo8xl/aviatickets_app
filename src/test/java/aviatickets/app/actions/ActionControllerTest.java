package aviatickets.app.actions;

import aviatickets.app.customer.entity.Customer;
import aviatickets.app.customer.entity.Role;
import aviatickets.app.jwt.JwtService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import static org.junit.jupiter.api.Assertions.assertNotNull;

@RunWith(SpringRunner.class)
@SpringBootTest
@AutoConfigureMockMvc
class ActionControllerTest {

	JwtService jwtService;
	Customer customer = new Customer();
	String authToken;

	@Autowired
	MockMvc mockMvc;

	@BeforeEach
	void setUp() {
		this.jwtService = new JwtService();
		this.customer.setCustomer(
				1,
				"user1",
				"email@example.com",
				"qwerty123456",
				false,
				false
		);
		this.authToken = this.jwtService.generateToken(customer);
	}

	@Test
	void testGetRequest() throws Exception {
		assertNotNull(this.authToken);

		this.mockMvc.perform(MockMvcRequestBuilders.get("/action/get-action-list/0/10/1/")
			.header("Authorization", this.authToken))
			.andExpect(MockMvcResultMatchers.status().isOk());
	}

}