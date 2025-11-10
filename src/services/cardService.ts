import {
  Card,
  CardListResponse,
  CardFilterCriteria,
  CardCreateRequest,
  CardUpdateRequest,
} from '@/types/card';

const API_BASE_URL = '/api';

/**
 * Card Service
 * Handles all API calls related to credit card management
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
   * @param criteria - Filter criteria including accountId, cardNumber, page, size
   * @returns Paginated card list response
   */
  async getCardsList(criteria: CardFilterCriteria = {}): Promise<CardListResponse> {
    const queryParams = new URLSearchParams();
    
    if (criteria.accountId) {
      queryParams.append('accountId', criteria.accountId);
    }
    if (criteria.cardNumber) {
      queryParams.append('cardNumber', criteria.cardNumber);
    }
    if (criteria.page !== undefined) {
      queryParams.append('page', criteria.page.toString());
    }
    if (criteria.size !== undefined) {
      queryParams.append('size', criteria.size.toString());
    }
    if (criteria.sort) {
      queryParams.append('sort', criteria.sort);
    }

    const url = `${API_BASE_URL}/cards?${queryParams.toString()}`;
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      throw new Error('Failed to fetch cards');
    }

    return response.json();
  }

  /**
   * Get card by card number
   * @param cardNumber - 16-digit card number
   * @returns Card details
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
   * Get all cards for a specific account
   * @param accountId - 11-digit account identifier
   * @returns Array of cards
   */
  async getCardsByAccount(accountId: string): Promise<Card[]> {
    const response = await fetch(`${API_BASE_URL}/cards/account/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      throw new Error('Failed to fetch cards by account');
    }

    return response.json();
  }

  /**
   * Create a new card
   * @param data - Card creation data
   * @returns Created card
   */
  async createCard(data: CardCreateRequest): Promise<Card> {
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
   * @param cardNumber - 16-digit card number
   * @param data - Card update data
   * @returns Updated card
   */
  async updateCard(cardNumber: string, data: CardUpdateRequest): Promise<Card> {
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
   * @param cardNumber - 16-digit card number
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
