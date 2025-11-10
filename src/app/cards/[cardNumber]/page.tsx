'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { Card } from '@/types/card';
import { Button } from '@/components/ui';

/**
 * Card Detail View (COCRDSLC)
 * 
 * Purpose: Display detailed information for a specific credit card
 * 
 * Features:
 * - View all card details
 * - Navigate to edit page
 * - Return to card list
 * - Delete card functionality
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

  const fetchCard = async (cardNumber: string) => {
    try {
      setLoading(true);
      const data = await cardService.getCardByNumber(cardNumber);
      setCard(data);
      setError(null);
    } catch (err) {
      setError('Failed to load card details');
      console.error('Failed to load card:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this card?')) return;

    try {
      await cardService.deleteCard(params.cardNumber as string);
      router.push('/cards');
    } catch (err) {
      alert('Failed to delete card');
      console.error(err);
    }
  };

  const handleBack = () => {
    router.push('/cards');
  };

  const handleEdit = () => {
    router.push(`/cards/${params.cardNumber}/edit`);
  };

  if (loading) {
    return (
      <div className="p-6 flex items-center justify-center min-h-screen">
        <div className="text-lg">Loading card details...</div>
      </div>
    );
  }

  if (error || !card) {
    return (
      <div className="p-6">
        <div className="bg-red-100 text-red-800 p-4 rounded-lg mb-6">
          {error || 'Card not found'}
        </div>
        <Button variant="secondary" onClick={handleBack}>
          Back to Card List
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-4xl mx-auto">
      {/* Header */}
      <div className="bg-blue-900 text-white p-4 mb-6 rounded-lg">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">CARD DETAIL VIEW</h1>
            <p className="text-sm">Transaction: COCRDSLC</p>
          </div>
        </div>
      </div>

      {/* Action Buttons */}
      <div className="flex justify-between items-center mb-6">
        <h2 className="text-xl font-bold">Card Information</h2>
        <div className="flex gap-2">
          <Button onClick={handleEdit}>Edit Card</Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete Card
          </Button>
          <Button variant="secondary" onClick={handleBack}>
            Back to List
          </Button>
        </div>
      </div>

      {/* Card Details */}
      <div className="bg-white shadow rounded-lg p-6 space-y-6">
        {/* Card Number */}
        <div className="border-b pb-4">
          <label className="block text-sm font-semibold text-gray-700 mb-1">
            Card Number
          </label>
          <p className="text-lg font-mono text-gray-900">{card.cardNumber}</p>
        </div>

        {/* Account ID */}
        <div className="border-b pb-4">
          <label className="block text-sm font-semibold text-gray-700 mb-1">
            Account ID
          </label>
          <p className="text-lg font-mono text-gray-900">{card.accountId}</p>
        </div>

        {/* Embossed Name */}
        <div className="border-b pb-4">
          <label className="block text-sm font-semibold text-gray-700 mb-1">
            Embossed Name
          </label>
          <p className="text-lg text-gray-900">{card.embossedName}</p>
        </div>

        {/* Expiration Date */}
        <div className="border-b pb-4">
          <label className="block text-sm font-semibold text-gray-700 mb-1">
            Expiration Date
          </label>
          <p className="text-lg text-gray-900">
            {new Date(card.expirationDate).toLocaleDateString('en-US', {
              year: 'numeric',
              month: 'long',
              day: 'numeric',
            })}
          </p>
          {card.expired && (
            <p className="text-sm text-red-600 mt-1">⚠️ This card has expired</p>
          )}
        </div>

        {/* Active Status */}
        <div className="border-b pb-4">
          <label className="block text-sm font-semibold text-gray-700 mb-1">
            Status
          </label>
          <div className="flex items-center gap-2">
            <span
              className={`px-3 py-1 text-sm font-semibold rounded ${
                card.active
                  ? 'bg-green-100 text-green-800'
                  : 'bg-gray-100 text-gray-800'
              }`}
            >
              {card.active ? 'Active' : 'Inactive'}
            </span>
            <span className="text-sm text-gray-600">
              ({card.activeStatus})
            </span>
          </div>
        </div>

        {/* Card State Summary */}
        <div className="bg-gray-50 p-4 rounded-lg">
          <h3 className="font-semibold text-gray-700 mb-2">Card State</h3>
          <div className="grid grid-cols-2 gap-4 text-sm">
            <div>
              <span className="text-gray-600">Active:</span>
              <span className="ml-2 font-semibold">
                {card.active ? 'Yes' : 'No'}
              </span>
            </div>
            <div>
              <span className="text-gray-600">Expired:</span>
              <span className="ml-2 font-semibold">
                {card.expired ? 'Yes' : 'No'}
              </span>
            </div>
          </div>
        </div>
      </div>

      {/* Footer Actions */}
      <div className="mt-6 flex justify-end gap-2">
        <Button onClick={handleEdit}>Edit Card</Button>
        <Button variant="secondary" onClick={handleBack}>
          Back to List
        </Button>
      </div>
    </div>
  );
}
