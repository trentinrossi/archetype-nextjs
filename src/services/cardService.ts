import {
  Card,
  CardListResponse,
  CardListFilters,
  CreateCardRequest,
  UpdateCardRequest,
} from '@/types/card';

const API_BASE_URL = '/api';

/**
 * Card Service
 * Handles all card-related API calls
 * Based on business rules for Credit Card List (CCRDLIA)
 */
class CardService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  /**
   * Get paginated list of cards with optional filtering
   * Default page size is 7 (as per COBOL screen specification)
   */
  async getCards(filters?: CardListFilters): Promise<CardListResponse> {
    const queryParams = new URLSearchParams();
    
    if (filters?.accountId) {
      queryParams.append('accountId', filters.accountId);
    }
    if (filters?.cardNumber) {
      queryParams.append('cardNumber', filters.cardNumber);
    }
    queryParams.append('page', String(filters?.page ?? 0));
    queryParams.append('size', String(filters?.size ?? 7));

    const response = await fetch(
      `${API_BASE_URL}/cards?${queryParams.toString()}`,
      {
        method: 'GET',
        headers: this.getAuthHeaders(),
      }
    );

    if (!response.ok) {
      throw new Error('Failed to fetch cards');
    }

    return response.json();
  }

  /**
   * Get card by card number
   */
  async getCardByNumber(cardNumber: string): Promise<Card> {
    const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      throw new Error('Failed to fetch card');
    }

    return response.json();
  }

  /**
   * Create a new card
   */
  async createCard(data: CreateCardRequest): Promise<Card> {
    const response = await fetch(`${API_BASE_URL}/cards`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });

    if (!response.ok) {
      throw new Error('Failed to create card');
    }

    return response.json();
  }

  /**
   * Update an existing card
   */
  async updateCard(
    cardNumber: string,
    data: UpdateCardRequest
  ): Promise<Card> {
    const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });

    if (!response.ok) {
      throw new Error('Failed to update card');
    }

    return response.json();
  }

  /**
   * Delete a card
   */
  async deleteCard(cardNumber: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      throw new Error('Failed to delete card');
    }
  }
}

export const cardService = new CardService();
