'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { Card } from '@/types/card';
import { Button } from '@/components/ui';

/**
 * Card Detail View Screen (COCRDSLC)
 * Displays detailed information for a single credit card
 * 
 * Business Rules:
 * - Shows all card details including account association
 * - Displays active/inactive status
 * - Shows expiration status
 * - Allows navigation to edit screen
 * - Allows return to card list
 */
export default function CardDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [card, setCard] = useState<Card | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (params.cardNumber) {
      fetchCard(params.cardNumber as string);
    }
  }, [params.cardNumber]);

  /**
   * Fetch card details by card number
   */
  const fetchCard = async (cardNumber: string) => {
    try {
      setLoading(true);
      setError(null);
      const data = await cardService.getCardByNumber(cardNumber);
      setCard(data);
    } catch (err) {
      setError('Failed to load card details');
      console.error('Failed to load card:', err);
    } finally {
      setLoading(false);
    }
  };

  /**
   * Handle navigation to edit screen
   */
  const handleEdit = () => {
    if (card) {
      router.push(`/credit-cards/${card.cardNumber}/edit`);
    }
  };

  /**
   * Handle return to card list
   */
  const handleBack = () => {
    router.push('/credit-cards');
  };

  /**
   * Handle delete card
   */
  const handleDelete = async () => {
    if (!card) return;
    
    if (!confirm(`Are you sure you want to delete card ${card.cardNumber}?`)) {
      return;
    }
    
    try {
      await cardService.deleteCard(card.cardNumber);
      router.push('/credit-cards');
    } catch (err) {
      alert('Failed to delete card');
      console.error(err);
    }
  };

  /**
   * Format date for display
   */
  const formatDate = (dateString: string): string => {
    try {
      const date = new Date(dateString);
      return date.toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'long',
        day: 'numeric',
      });
    } catch {
      return dateString;
    }
  };

  /**
   * Check if card is expired
   */
  const isExpired = (expirationDate: string): boolean => {
    try {
      const expDate = new Date(expirationDate);
      const today = new Date();
      return expDate < today;
    } catch {
      return false;
    }
  };

  if (loading) {
    return <div className="p-6">Loading card details...</div>;
  }

  if (error || !card) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-300 text-red-800 px-4 py-3 rounded mb-4">
          {error || 'Card not found'}
        </div>
        <Button onClick={handleBack} variant="secondary">
          Back to Card List
        </Button>
      </div>
    );
  }

  const expired = isExpired(card.expirationDate);

  return (
    <div className="p-6 max-w-4xl mx-auto">
      {/* Header */}
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">Card Details</h1>
          <p className="text-sm text-gray-600 mt-1">COCRDSLC - Card Detail View</p>
        </div>
        <div className="flex gap-2">
          <Button onClick={handleEdit}>
            Edit Card
          </Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete Card
          </Button>
          <Button variant="secondary" onClick={handleBack}>
            Back to List
          </Button>
        </div>
      </div>

      {/* Card Information */}
      <div className="bg-white shadow rounded-lg overflow-hidden">
        {/* Status Banner */}
        <div
          className={`px-6 py-3 ${
            card.activeStatus === 'Y' && !expired
              ? 'bg-green-100 border-b border-green-200'
              : 'bg-red-100 border-b border-red-200'
          }`}
        >
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-4">
              <span
                className={`inline-flex px-3 py-1 text-sm font-semibold rounded-full ${
                  card.activeStatus === 'Y'
                    ? 'bg-green-200 text-green-900'
                    : 'bg-red-200 text-red-900'
                }`}
              >
                {card.activeStatus === 'Y' ? 'Active' : 'Inactive'}
              </span>
              {expired && (
                <span className="inline-flex px-3 py-1 text-sm font-semibold rounded-full bg-red-200 text-red-900">
                  Expired
                </span>
              )}
            </div>
          </div>
        </div>

        {/* Card Details */}
        <div className="px-6 py-6 space-y-6">
          {/* Card Number Section */}
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Card Number
              </label>
              <p className="text-lg font-mono text-gray-900 bg-gray-50 px-3 py-2 rounded border border-gray-200">
                {card.cardNumber}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Account ID
              </label>
              <p className="text-lg font-mono text-gray-900 bg-gray-50 px-3 py-2 rounded border border-gray-200">
                {card.accountId}
              </p>
            </div>
          </div>

          {/* Cardholder Information */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Embossed Name
            </label>
            <p className="text-lg text-gray-900 bg-gray-50 px-3 py-2 rounded border border-gray-200">
              {card.embossedName}
            </p>
          </div>

          {/* Dates Section */}
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Expiration Date
              </label>
              <p
                className={`text-lg text-gray-900 bg-gray-50 px-3 py-2 rounded border ${
                  expired ? 'border-red-300 bg-red-50' : 'border-gray-200'
                }`}
              >
                {formatDate(card.expirationDate)}
                {expired && (
                  <span className="ml-2 text-sm text-red-600 font-semibold">
                    (Expired)
                  </span>
                )}
              </p>
            </div>
          </div>

          {/* Status Information */}
          <div className="border-t border-gray-200 pt-6">
            <h3 className="text-lg font-semibold text-gray-900 mb-4">Status Information</h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div className="flex items-center">
                <span className="text-sm font-medium text-gray-700 mr-2">Active:</span>
                <span
                  className={`text-sm font-semibold ${
                    card.active ? 'text-green-600' : 'text-red-600'
                  }`}
                >
                  {card.active ? 'Yes' : 'No'}
                </span>
              </div>
              <div className="flex items-center">
                <span className="text-sm font-medium text-gray-700 mr-2">Expired:</span>
                <span
                  className={`text-sm font-semibold ${
                    card.expired ? 'text-red-600' : 'text-green-600'
                  }`}
                >
                  {card.expired ? 'Yes' : 'No'}
                </span>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Action Buttons (Bottom) */}
      <div className="mt-6 flex gap-3">
        <Button onClick={handleEdit}>
          Edit Card Information
        </Button>
        <Button variant="secondary" onClick={handleBack}>
          Return to Card List
        </Button>
      </div>

      {/* Instructions */}
      <div className="mt-6 p-4 bg-gray-50 rounded-lg border border-gray-200">
        <h3 className="text-sm font-semibold text-gray-900 mb-2">Available Actions:</h3>
        <ul className="text-sm text-gray-700 space-y-1">
          <li>• Click <strong>Edit Card</strong> to modify card information</li>
          <li>• Click <strong>Delete Card</strong> to permanently remove this card</li>
          <li>• Click <strong>Return to Card List</strong> to go back to the card list screen</li>
        </ul>
      </div>
    </div>
  );
}
