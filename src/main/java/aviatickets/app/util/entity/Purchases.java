package aviatickets.app.util.entity;

import aviatickets.app.purchase.dto.request.CreatePurchaseDto;

import java.util.List;

public record Purchases(List<CreatePurchaseDto> purchaseList) {
}
