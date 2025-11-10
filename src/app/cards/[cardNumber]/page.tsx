'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { Card } from '@/types/card';
import { Button } from '@/components/ui';

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

  const formatDate = (dateString: string) => {
    try {
      return new Date(dateString).toLocaleDateString();
    } catch {
      return dateString;
    }
  };

  if (loading) {
    return <div className="p-6">Loading...</div>;
  }

  if (error || !card) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-4">
          {error || 'Card not found'}
        </div>
        <Button variant="secondary" onClick={() => router.push('/cards')}>
          Back to Card List
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6">
      {/* Header Section */}
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Card Details</h1>
          <p className="text-sm text-gray-600">COCRDSLC - Credit Card Detail View</p>
        </div>
        <div className="flex gap-2">
          <Button onClick={() => router.push(`/cards/${card.cardNumber}/edit`)}>
            Edit Card
          </Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete Card
          </Button>
          <Button variant="secondary" onClick={() => router.push('/cards')}>
            Back to List
          </Button>
        </div>
      </div>

      {/* Card Information Section */}
      <div className="bg-white shadow rounded-lg p-6 space-y-6">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          {/* Card Number */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Card Number
            </label>
            <p className="text-gray-900 text-lg font-mono">{card.cardNumber}</p>
          </div>

          {/* Account ID */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Account ID
            </label>
            <p className="text-gray-900 text-lg font-mono">{card.accountId}</p>
          </div>

          {/* Embossed Name */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Embossed Name
            </label>
            <p className="text-gray-900">{card.embossedName}</p>
          </div>

          {/* Expiration Date */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Expiration Date
            </label>
            <p className="text-gray-900">{formatDate(card.expirationDate)}</p>
          </div>

          {/* Active Status */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Status
            </label>
            <div className="flex items-center gap-2">
              <span
                className={`inline-flex items-center px-3 py-1 rounded-full text-sm font-medium ${
                  card.active
                    ? 'bg-green-100 text-green-800'
                    : 'bg-red-100 text-red-800'
                }`}
              >
                {card.active ? 'Active' : 'Inactive'}
              </span>
              {card.expired && (
                <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-yellow-100 text-yellow-800">
                  Expired
                </span>
              )}
            </div>
          </div>

          {/* Raw Status Code */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Status Code
            </label>
            <p className="text-gray-900 font-mono">{card.activeStatus}</p>
          </div>
        </div>

        {/* Expired Warning */}
        {card.expired && (
          <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded">
            <p className="font-semibold">⚠️ This card has expired</p>
            <p className="text-sm mt-1">
              The card expired on {formatDate(card.expirationDate)}. Please issue a new card.
            </p>
          </div>
        )}

        {/* Inactive Warning */}
        {!card.active && !card.expired && (
          <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded">
            <p className="font-semibold">⚠️ This card is inactive</p>
            <p className="text-sm mt-1">
              The card is currently inactive and cannot be used for transactions.
            </p>
          </div>
        )}
      </div>

      {/* Action Buttons */}
      <div className="flex gap-2 mt-6">
        <Button onClick={() => router.push(`/cards/${card.cardNumber}/edit`)}>
          Edit Card
        </Button>
        <Button variant="secondary" onClick={() => router.push('/cards')}>
          Back to Card List
        </Button>
      </div>
    </div>
  );
}
