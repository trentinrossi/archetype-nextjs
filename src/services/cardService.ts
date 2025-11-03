// src/services/cardService.ts

import { Card, CreateCardData, UpdateCardData, ApiError } from '@/types/account';

const API_BASE_URL = '/api';

class CardService {
  private getAuthHeaders(): Record<string, string> {
    const token = typeof window !== 'undefined' ? localStorage.getItem('access_token') : null;
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  private async handleResponse<T>(response: Response): Promise<T> {
    if (!response.ok) {
      const errorData: ApiError = await response.json().catch(() => ({
        error: 'An unexpected error occurred',
        status: response.status,
      }));
      throw new Error(errorData.error || `HTTP error! status: ${response.status}`);
    }
    return response.json();
  }

  async getCards(): Promise<Card[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Card[]>(response);
    } catch (error) {
      console.error('Error fetching cards:', error);
      throw error;
    }
  }

  async getCardByCardNumber(cardNumber: string): Promise<Card> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Card>(response);
    } catch (error) {
      console.error(`Error fetching card ${cardNumber}:`, error);
      throw error;
    }
  }

  async getCardsByAccountId(accountId: string): Promise<Card[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards/account/${accountId}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Card[]>(response);
    } catch (error) {
      console.error(`Error fetching cards by account ${accountId}:`, error);
      throw error;
    }
  }

  async getCardsByStatus(activeStatus: string): Promise<Card[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards/status/${activeStatus}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Card[]>(response);
    } catch (error) {
      console.error(`Error fetching cards by status ${activeStatus}:`, error);
      throw error;
    }
  }

  async createCard(cardData: CreateCardData): Promise<Card> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(cardData),
      });
      return this.handleResponse<Card>(response);
    } catch (error) {
      console.error('Error creating card:', error);
      throw error;
    }
  }

  async updateCard(cardNumber: string, cardData: UpdateCardData): Promise<Card> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(cardData),
      });
      return this.handleResponse<Card>(response);
    } catch (error) {
      console.error(`Error updating card ${cardNumber}:`, error);
      throw error;
    }
  }

  async deleteCard(cardNumber: string): Promise<void> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      });
      if (!response.ok) {
        const errorData: ApiError = await response.json().catch(() => ({
          error: 'An unexpected error occurred',
          status: response.status,
        }));
        throw new Error(errorData.error || `HTTP error! status: ${response.status}`);
      }
    } catch (error) {
      console.error(`Error deleting card ${cardNumber}:`, error);
      throw error;
    }
  }
}

export const cardService = new CardService();
export default CardService;
