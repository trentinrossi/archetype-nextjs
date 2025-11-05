'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Table } from '@/components/ui';
import { Card } from '@/types/card';
import { cardService } from '@/services/cardService';

export default function CardsPage() {
  const router = useRouter();
  const [cards, setCards] = useState<Card[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(1);

  useEffect(() => {
    fetchCards();
  }, [currentPage]);

  const fetchCards = async () => {
    try {
      setLoading(true);
      setError(null);
      const response = await cardService.getCards(currentPage, 20);
      setCards(response.content || []);
      setTotalPages(response.totalPages || 1);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load cards');
      console.error('Error fetching cards:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async (cardNumber: string) => {
    if (!confirm('Are you sure you want to delete this card?')) return;
    try {
      await cardService.deleteCard(cardNumber);
      fetchCards();
    } catch (err) {
      alert('Failed to delete card');
      console.error('Error deleting card:', err);
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600"></div>
          <p className="mt-4 text-gray-600">Loading cards...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">Cards</h1>
          <p className="text-sm text-gray-600 mt-1">View and manage cards</p>
        </div>
        <Button onClick={() => router.push('/cards/new')}>Create Card</Button>
      </div>

      {error && (
        <div className="mb-6 bg-red-50 border border-red-200 rounded-lg p-4">
          <p className="text-sm text-red-700">{error}</p>
        </div>
      )}

      {cards.length === 0 ? (
        <div className="bg-white shadow rounded-lg p-12 text-center">
          <h3 className="text-sm font-medium text-gray-900">No cards found</h3>
          <p className="mt-1 text-sm text-gray-500">Get started by creating a new card.</p>
        </div>
      ) : (
        <>
          <div className="bg-white shadow rounded-lg overflow-hidden">
            <Table
              columns={[
                { key: 'cardNumber', label: 'Card Number', render: (val) => `****${(val as string).slice(-4)}` },
                { key: 'status', label: 'Status' },
                { key: 'cardDetails', label: 'Details' },
                { key: 'createdAt', label: 'Created', render: (val) => new Date(val as string).toLocaleDateString() },
              ]}
              data={cards}
              actions={(card) => (
                <div className="flex gap-2">
                  <Button size="sm" onClick={() => router.push(`/cards/${card.cardNumber}`)}>View</Button>
                  <Button size="sm" variant="danger" onClick={() => handleDelete(card.cardNumber)}>Delete</Button>
                </div>
              )}
            />
          </div>

          {totalPages > 1 && (
            <div className="mt-6 bg-white shadow rounded-lg p-4 flex justify-between items-center">
              <div className="text-sm text-gray-700">Page {currentPage + 1} of {totalPages}</div>
              <div className="flex gap-2">
                <Button variant="secondary" size="sm" onClick={() => setCurrentPage(currentPage - 1)} disabled={currentPage === 0}>Previous</Button>
                <Button variant="secondary" size="sm" onClick={() => setCurrentPage(currentPage + 1)} disabled={currentPage >= totalPages - 1}>Next</Button>
              </div>
            </div>
          )}
        </>
      )}
    </div>
  );
}
