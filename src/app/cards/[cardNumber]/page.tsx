'use client';

import React, { useEffect, useState } from 'react';
import { useRouter, useParams } from 'next/navigation';
import { Button } from '@/components/ui';
import { Card } from '@/types/card';
import { cardService } from '@/services/cardService';

export default function CardDetailPage() {
  const router = useRouter();
  const params = useParams();
  const cardNumber = params.cardNumber as string;

  const [card, setCard] = useState<Card | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (cardNumber) {
      fetchCard();
    }
  }, [cardNumber]);

  const fetchCard = async () => {
    try {
      setLoading(true);
      setError(null);
      const data = await cardService.getCardByCardNumber(cardNumber);
      setCard(data);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load card');
      console.error('Error fetching card:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this card?')) return;

    try {
      await cardService.deleteCard(cardNumber);
      alert('Card deleted successfully');
      router.push('/cards');
    } catch (err) {
      alert('Failed to delete card');
      console.error('Error deleting card:', err);
    }
  };

  const formatTimestamp = (timestamp: string): string => {
    return new Date(timestamp).toLocaleString('en-US', {
      year: 'numeric',
      month: 'long',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
    });
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600"></div>
          <p className="mt-4 text-gray-600">Loading card...</p>
        </div>
      </div>
    );
  }

  if (error || !card) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <div className="flex">
            <div className="flex-shrink-0">
              <svg className="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
                <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
              </svg>
            </div>
            <div className="ml-3">
              <h3 className="text-sm font-medium text-red-800">Error loading card</h3>
              <p className="mt-1 text-sm text-red-700">{error || 'Card not found'}</p>
            </div>
          </div>
          <div className="mt-4">
            <Button variant="secondary" onClick={() => router.push('/cards')}>
              Back to Cards
            </Button>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-5xl mx-auto">
      <div className="mb-6">
        <button
          onClick={() => router.back()}
          className="text-blue-600 hover:text-blue-800 flex items-center gap-2 mb-4"
        >
          <svg className="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M15 19l-7-7 7-7" />
          </svg>
          Back
        </button>
        <div className="flex justify-between items-start">
          <div>
            <h1 className="text-2xl font-bold text-gray-900">Card Details</h1>
            <p className="text-sm text-gray-600 mt-1">Card Number: **** **** **** {card.cardNumber.slice(-4)}</p>
          </div>
          <div className="flex gap-2">
            <Button variant="danger" onClick={handleDelete}>
              Delete Card
            </Button>
          </div>
        </div>
      </div>

      <div className="bg-white shadow rounded-lg overflow-hidden mb-6">
        <div className="px-6 py-4 border-b border-gray-200 bg-gray-50">
          <h2 className="text-lg font-semibold text-gray-900">Card Information</h2>
        </div>
        <div className="p-6">
          <dl className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <dt className="text-sm font-medium text-gray-500">Card Number</dt>
              <dd className="mt-1 text-sm text-gray-900">
                **** **** **** {card.cardNumber.slice(-4)}
              </dd>
            </div>
            <div>
              <dt className="text-sm font-medium text-gray-500">Status</dt>
              <dd className="mt-1">
                <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                  card.status === 'ACTIVE' ? 'bg-green-100 text-green-800' : 'bg-gray-100 text-gray-800'
                }`}>
                  {card.status}
                </span>
              </dd>
            </div>
            {card.cardDetails && (
              <div className="md:col-span-2">
                <dt className="text-sm font-medium text-gray-500">Card Details</dt>
                <dd className="mt-1 text-sm text-gray-900">{card.cardDetails}</dd>
              </div>
            )}
          </dl>
        </div>
      </div>

      <div className="bg-white shadow rounded-lg overflow-hidden">
        <div className="px-6 py-4 border-b border-gray-200 bg-gray-50">
          <h2 className="text-lg font-semibold text-gray-900">Timestamps</h2>
        </div>
        <div className="p-6">
          <dl className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <dt className="text-sm font-medium text-gray-500">Created At</dt>
              <dd className="mt-1 text-sm text-gray-900">{formatTimestamp(card.createdAt)}</dd>
            </div>
            <div>
              <dt className="text-sm font-medium text-gray-500">Updated At</dt>
              <dd className="mt-1 text-sm text-gray-900">{formatTimestamp(card.updatedAt)}</dd>
            </div>
          </dl>
        </div>
      </div>
    </div>
  );
}
