'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { CardListResponse, CardListFilters } from '@/types/card';
import { Table, Button, Input } from '@/components/ui';

export default function CardsPage() {
  const router = useRouter();
  const [cardsData, setCardsData] = useState<CardListResponse | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [filters, setFilters] = useState<CardListFilters>({
    accountId: '',
    cardNumber: '',
    page: 0,
    size: 7,
  });
  const [filterErrors, setFilterErrors] = useState<{
    accountId?: string;
    cardNumber?: string;
  }>({});

  useEffect(() => {
    fetchCards();
  }, [filters.page]);

  const validateFilters = (): boolean => {
    const errors: { accountId?: string; cardNumber?: string } = {};
    let isValid = true;

    if (filters.accountId && filters.accountId.trim() !== '') {
      if (!/^\d{11}$/.test(filters.accountId.trim())) {
        errors.accountId = 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER';
        isValid = false;
      }
    }

    if (filters.cardNumber && filters.cardNumber.trim() !== '') {
      if (!/^\d{16}$/.test(filters.cardNumber.trim())) {
        errors.cardNumber = 'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER';
        isValid = false;
      }
    }

    setFilterErrors(errors);
    return isValid;
  };

  const fetchCards = async () => {
    try {
      setLoading(true);
      setError(null);
      
      const cleanFilters: CardListFilters = {
        page: filters.page,
        size: filters.size,
      };

      if (filters.accountId && filters.accountId.trim() !== '') {
        cleanFilters.accountId = filters.accountId.trim();
      }

      if (filters.cardNumber && filters.cardNumber.trim() !== '') {
        cleanFilters.cardNumber = filters.cardNumber.trim();
      }

      const data = await cardService.getCardsList(cleanFilters);
      setCardsData(data);

      if (data.content.length === 0) {
        setError('NO RECORDS FOUND FOR THIS SEARCH CONDITION');
      }
    } catch (err) {
      setError('Failed to load cards');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = () => {
    if (validateFilters()) {
      setFilters({ ...filters, page: 0 });
      fetchCards();
    }
  };

  const handlePreviousPage = () => {
    if (cardsData && !cardsData.first) {
      setFilters({ ...filters, page: filters.page! - 1 });
    } else {
      setError('NO PREVIOUS PAGES TO DISPLAY');
    }
  };

  const handleNextPage = () => {
    if (cardsData && !cardsData.last) {
      setFilters({ ...filters, page: filters.page! + 1 });
    } else {
      setError('NO MORE PAGES TO DISPLAY');
    }
  };

  const handleDelete = async (cardNumber: string) => {
    if (!confirm('Are you sure you want to delete this card?')) return;

    try {
      await cardService.deleteCard(cardNumber);
      fetchCards();
    } catch (err) {
      alert('Failed to delete card');
      console.error(err);
    }
  };

  const handleViewDetails = (cardNumber: string) => {
    router.push(`/cards/${cardNumber}`);
  };

  const handleEdit = (cardNumber: string) => {
    router.push(`/cards/${cardNumber}/edit`);
  };

  const currentDate = new Date().toLocaleDateString();
  const currentTime = new Date().toLocaleTimeString();

  return (
    <div className="p-6">
      {/* Header Section */}
      <div className="mb-6">
        <div className="flex justify-between items-start mb-2">
          <div>
            <h1 className="text-2xl font-bold">Credit Card List</h1>
            <p className="text-sm text-gray-600">COCRDLIC - Credit Card List Program</p>
          </div>
          <div className="text-right text-sm text-gray-600">
            <p>{currentDate}</p>
            <p>{currentTime}</p>
            {cardsData && (
              <p>Page {cardsData.number + 1} of {cardsData.totalPages || 1}</p>
            )}
          </div>
        </div>
      </div>

      {/* Search Criteria Section */}
      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-lg font-semibold mb-4">Search Criteria</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div>
            <Input
              label="Account ID (11 digits)"
              value={filters.accountId || ''}
              onChange={(e) => {
                setFilters({ ...filters, accountId: e.target.value });
                setFilterErrors({ ...filterErrors, accountId: undefined });
              }}
              placeholder="Enter 11-digit account ID"
              error={filterErrors.accountId}
            />
          </div>
          <div>
            <Input
              label="Card Number (16 digits)"
              value={filters.cardNumber || ''}
              onChange={(e) => {
                setFilters({ ...filters, cardNumber: e.target.value });
                setFilterErrors({ ...filterErrors, cardNumber: undefined });
              }}
              placeholder="Enter 16-digit card number"
              error={filterErrors.cardNumber}
            />
          </div>
        </div>
        <div className="flex gap-2 mt-4">
          <Button onClick={handleSearch}>
            Search
          </Button>
          <Button
            variant="secondary"
            onClick={() => {
              setFilters({ accountId: '', cardNumber: '', page: 0, size: 7 });
              setFilterErrors({});
            }}
          >
            Clear Filters
          </Button>
          <Button onClick={() => router.push('/cards/new')}>
            Create New Card
          </Button>
        </div>
      </div>

      {/* Messages Section */}
      {error && (
        <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded mb-4">
          {error}
        </div>
      )}

      {/* Card List Section */}
      {loading ? (
        <div className="text-center py-8">Loading...</div>
      ) : cardsData && cardsData.content.length > 0 ? (
        <>
          <div className="bg-white shadow rounded-lg overflow-hidden">
            <Table
              columns={[
                { key: 'accountId', label: 'Account Number' },
                { key: 'cardNumber', label: 'Card Number' },
                { key: 'activeStatus', label: 'Status' },
              ]}
              data={cardsData.content.map(card => ({
                ...card,
                activeStatus: card.activeStatus === 'Y' ? 'Active' : 'Inactive',
              }))}
              onRowClick={(card) => handleViewDetails(card.cardNumber)}
              actions={(card) => (
                <div className="flex gap-2">
                  <Button
                    size="sm"
                    onClick={(e) => {
                      e.stopPropagation();
                      handleViewDetails(card.cardNumber);
                    }}
                  >
                    View
                  </Button>
                  <Button
                    size="sm"
                    onClick={(e) => {
                      e.stopPropagation();
                      handleEdit(card.cardNumber);
                    }}
                  >
                    Edit
                  </Button>
                  <Button
                    size="sm"
                    variant="danger"
                    onClick={(e) => {
                      e.stopPropagation();
                      handleDelete(card.cardNumber);
                    }}
                  >
                    Delete
                  </Button>
                </div>
              )}
            />
          </div>

          {/* Pagination Controls */}
          <div className="flex justify-between items-center mt-4">
            <div className="text-sm text-gray-600">
              Showing {cardsData.content.length} of {cardsData.totalElements} cards
            </div>
            <div className="flex gap-2">
              <Button
                variant="secondary"
                onClick={handlePreviousPage}
                disabled={cardsData.first}
              >
                Previous Page
              </Button>
              <Button
                variant="secondary"
                onClick={handleNextPage}
                disabled={cardsData.last}
              >
                Next Page
              </Button>
            </div>
          </div>
        </>
      ) : null}

      {/* Navigation Actions */}
      <div className="flex gap-2 mt-6">
        <Button variant="secondary" onClick={() => router.push('/')}>
          Return to Main Menu
        </Button>
      </div>
    </div>
  );
}
