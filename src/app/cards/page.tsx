'use client';

import React, { useEffect, useState } from 'react';
import { Card } from '@/types/account';
import { Table, Button, Input, Select, Modal } from '@/components/ui';
import { useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';

const CardsPage: React.FC = () => {
  const router = useRouter();
  const [cards, setCards] = useState<Card[]>([]);
  const [filteredCards, setFilteredCards] = useState<Card[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchTerm, setSearchTerm] = useState('');
  const [statusFilter, setStatusFilter] = useState<string>('all');
  const [deleteModalOpen, setDeleteModalOpen] = useState(false);
  const [cardToDelete, setCardToDelete] = useState<string | null>(null);
  const [deleting, setDeleting] = useState(false);

  const fetchCards = async () => {
    try {
      setLoading(true);
      setError(null);
      const data = await cardService.getCards();
      setCards(data);
      setFilteredCards(data);
    } catch (err) {
      console.error('Error fetching cards:', err);
      setError(err instanceof Error ? err.message : 'An unexpected error occurred');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchCards();
  }, []);

  useEffect(() => {
    let filtered = [...cards];

    if (searchTerm.trim() !== '') {
      const term = searchTerm.toLowerCase();
      filtered = filtered.filter(card =>
        card.cardNumber.toLowerCase().includes(term) ||
        card.accountId.toLowerCase().includes(term) ||
        card.embossedName.toLowerCase().includes(term)
      );
    }

    if (statusFilter !== 'all') {
      filtered = filtered.filter(card => card.activeStatus === statusFilter);
    }

    setFilteredCards(filtered);
  }, [searchTerm, statusFilter, cards]);

  const handleViewDetails = (cardNumber: string) => {
    router.push(`/cards/${cardNumber}`);
  };

  const handleEdit = (cardNumber: string) => {
    router.push(`/cards/${cardNumber}/edit`);
  };

  const handleDeleteClick = (cardNumber: string) => {
    setCardToDelete(cardNumber);
    setDeleteModalOpen(true);
  };

  const handleDeleteConfirm = async () => {
    if (!cardToDelete) return;

    try {
      setDeleting(true);
      await cardService.deleteCard(cardToDelete);
      setCards(prev => prev.filter(card => card.cardNumber !== cardToDelete));
      setDeleteModalOpen(false);
      setCardToDelete(null);
    } catch (err) {
      console.error('Error deleting card:', err);
      setError(err instanceof Error ? err.message : 'Failed to delete card');
    } finally {
      setDeleting(false);
    }
  };

  const handleDeleteCancel = () => {
    setDeleteModalOpen(false);
    setCardToDelete(null);
  };

  const handleCreateNew = () => {
    router.push('/cards/new');
  };

  const formatDate = (dateString: string): string => {
    if (!dateString) return 'N/A';
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
    });
  };

  const maskCardNumber = (cardNumber: string): string => {
    if (!cardNumber || cardNumber.length < 4) return '****-****-****-****';
    return `****-****-****-${cardNumber.slice(-4)}`;
  };

  const columns = [
    {
      key: 'cardNumber',
      header: 'Card Number',
      render: (card: Card) => (
        <span className="font-medium text-gray-900 font-mono">{maskCardNumber(card.cardNumber)}</span>
      ),
    },
    {
      key: 'embossedName',
      header: 'Cardholder Name',
      render: (card: Card) => (
        <span className="text-gray-900">{card.embossedName}</span>
      ),
    },
    {
      key: 'accountId',
      header: 'Account ID',
      render: (card: Card) => (
        <span className="text-gray-900">{card.accountId}</span>
      ),
    },
    {
      key: 'expirationDate',
      header: 'Expiration Date',
      render: (card: Card) => (
        <span className="text-gray-900">{formatDate(card.expirationDate)}</span>
      ),
    },
    {
      key: 'activeStatus',
      header: 'Status',
      render: (card: Card) => (
        <span
          className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
            card.activeStatus === 'Y'
              ? 'bg-green-100 text-green-800'
              : 'bg-red-100 text-red-800'
          }`}
        >
          {card.activeStatus === 'Y' ? 'Active' : 'Inactive'}
        </span>
      ),
    },
    {
      key: 'actions',
      header: 'Actions',
      render: (card: Card) => (
        <div className="flex gap-2">
          <Button
            variant="secondary"
            size="sm"
            onClick={() => handleViewDetails(card.cardNumber)}
          >
            View
          </Button>
          <Button
            variant="secondary"
            size="sm"
            onClick={() => handleEdit(card.cardNumber)}
          >
            Edit
          </Button>
          <Button
            variant="danger"
            size="sm"
            onClick={() => handleDeleteClick(card.cardNumber)}
          >
            Delete
          </Button>
        </div>
      ),
    },
  ];

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading cards...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="text-red-600 text-xl mb-4">Error</div>
          <p className="text-gray-600 mb-4">{error}</p>
          <Button onClick={fetchCards}>Retry</Button>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Cards</h1>
        <p className="text-gray-600">Manage and view all credit cards</p>
      </div>

      <div className="mb-6 flex flex-wrap gap-4 items-end">
        <div className="flex-1 min-w-[300px]">
          <label htmlFor="search" className="block text-sm font-medium text-gray-700 mb-1">
            Search Cards
          </label>
          <Input
            id="search"
            type="text"
            placeholder="Search by card number, account ID, or name..."
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
          />
        </div>

        <div className="flex-1 min-w-[200px]">
          <label htmlFor="status-filter" className="block text-sm font-medium text-gray-700 mb-1">
            Filter by Status
          </label>
          <Select
            id="status-filter"
            value={statusFilter}
            onChange={(e) => setStatusFilter(e.target.value)}
            options={[
              { value: 'all', label: 'All Statuses' },
              { value: 'Y', label: 'Active' },
              { value: 'N', label: 'Inactive' },
            ]}
          />
        </div>

        <div className="flex-shrink-0">
          <Button onClick={handleCreateNew}>Create New Card</Button>
        </div>
      </div>

      {filteredCards.length === 0 ? (
        <div className="text-center py-12 bg-gray-50 rounded-lg">
          <svg
            className="mx-auto h-12 w-12 text-gray-400"
            fill="none"
            viewBox="0 0 24 24"
            stroke="currentColor"
            aria-hidden="true"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M3 10h18M7 15h1m4 0h1m-7 4h12a3 3 0 003-3V8a3 3 0 00-3-3H6a3 3 0 00-3 3v8a3 3 0 003 3z"
            />
          </svg>
          <h3 className="mt-2 text-sm font-medium text-gray-900">No cards found</h3>
          <p className="mt-1 text-sm text-gray-500">
            {cards.length === 0
              ? 'Get started by creating a new card.'
              : 'Try adjusting your search or filters to see more results.'}
          </p>
          {cards.length === 0 && (
            <div className="mt-6">
              <Button onClick={handleCreateNew}>Create New Card</Button>
            </div>
          )}
        </div>
      ) : (
        <div className="bg-white shadow-md rounded-lg overflow-hidden">
          <Table
            data={filteredCards}
            columns={columns}
            keyExtractor={(card) => card.cardNumber}
          />
          <div className="px-6 py-4 bg-gray-50 border-t border-gray-200">
            <p className="text-sm text-gray-700">
              Showing <span className="font-medium">{filteredCards.length}</span> of{' '}
              <span className="font-medium">{cards.length}</span> cards
            </p>
          </div>
        </div>
      )}

      <Modal
        isOpen={deleteModalOpen}
        onClose={handleDeleteCancel}
        title="Delete Card"
      >
        <div className="mt-2">
          <p className="text-sm text-gray-500">
            Are you sure you want to delete card ending in <strong>{cardToDelete?.slice(-4)}</strong>? This action cannot be undone.
          </p>
        </div>
        <div className="mt-6 flex justify-end gap-3">
          <Button
            variant="secondary"
            onClick={handleDeleteCancel}
            disabled={deleting}
          >
            Cancel
          </Button>
          <Button
            variant="danger"
            onClick={handleDeleteConfirm}
            disabled={deleting}
          >
            {deleting ? 'Deleting...' : 'Delete'}
          </Button>
        </div>
      </Modal>
    </div>
  );
};

export default CardsPage;
