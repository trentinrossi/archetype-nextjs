'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { CardListItem, PageResponse } from '@/types/cardServices';
import { cardService } from '@/services/cardService';
import { Table, Button, Input } from '@/components/ui';

export default function CardsListPage() {
  const router = useRouter();
  const [cards, setCards] = useState<CardListItem[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [totalElements, setTotalElements] = useState(0);
  const [accountIdFilter, setAccountIdFilter] = useState('');
  const [cardNumberFilter, setCardNumberFilter] = useState('');
  const [appliedAccountId, setAppliedAccountId] = useState('');
  const [appliedCardNumber, setAppliedCardNumber] = useState('');

  useEffect(() => {
    fetchCards();
  }, [currentPage, appliedAccountId, appliedCardNumber]);

  const fetchCards = async () => {
    try {
      setLoading(true);
      const params: any = {
        page: currentPage,
        size: 7,
        sort: 'cardNumber,asc',
      };

      if (appliedAccountId) {
        params.accountId = appliedAccountId;
      }

      if (appliedCardNumber) {
        params.cardNumber = appliedCardNumber;
      }

      const data: PageResponse<CardListItem> = await cardService.getCardsList(params);
      setCards(data.content);
      setTotalPages(data.totalPages);
      setTotalElements(data.totalElements);
      setError(null);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load cards');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async (cardNumber: string) => {
    if (!confirm('Are you sure you want to delete this card? This action cannot be undone.')) {
      return;
    }

    try {
      await cardService.deleteCard(cardNumber);
      fetchCards();
    } catch (err) {
      alert(err instanceof Error ? err.message : 'Failed to delete card');
      console.error(err);
    }
  };

  const handleSearch = () => {
    setAppliedAccountId(accountIdFilter);
    setAppliedCardNumber(cardNumberFilter);
    setCurrentPage(0);
  };

  const handleClearFilters = () => {
    setAccountIdFilter('');
    setCardNumberFilter('');
    setAppliedAccountId('');
    setAppliedCardNumber('');
    setCurrentPage(0);
  };

  const handlePageChange = (page: number) => {
    setCurrentPage(page);
  };

  const maskCardNumber = (cardNumber: string): string => {
    return `**** **** **** ${cardNumber.slice(-4)}`;
  };

  const formatDate = (dateString: string): string => {
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
    });
  };

  const renderPagination = () => {
    if (totalPages <= 1) return null;

    const pages: number[] = [];
    const maxVisiblePages = 5;
    let startPage = Math.max(0, currentPage - Math.floor(maxVisiblePages / 2));
    let endPage = Math.min(totalPages - 1, startPage + maxVisiblePages - 1);

    if (endPage - startPage < maxVisiblePages - 1) {
      startPage = Math.max(0, endPage - maxVisiblePages + 1);
    }

    for (let i = startPage; i <= endPage; i++) {
      pages.push(i);
    }

    return (
      <div className="flex items-center justify-between mt-6">
        <div className="text-sm text-gray-700">
          Showing <span className="font-medium">{currentPage * 7 + 1}</span> to{' '}
          <span className="font-medium">{Math.min((currentPage + 1) * 7, totalElements)}</span> of{' '}
          <span className="font-medium">{totalElements}</span> results
        </div>
        <div className="flex gap-2">
          <Button
            size="sm"
            variant="secondary"
            onClick={() => handlePageChange(currentPage - 1)}
            disabled={currentPage === 0}
          >
            Previous
          </Button>
          {startPage > 0 && (
            <>
              <Button
                size="sm"
                variant="secondary"
                onClick={() => handlePageChange(0)}
              >
                1
              </Button>
              {startPage > 1 && <span className="px-2 py-1">...</span>}
            </>
          )}
          {pages.map((page) => (
            <Button
              key={page}
              size="sm"
              variant={currentPage === page ? 'primary' : 'secondary'}
              onClick={() => handlePageChange(page)}
            >
              {page + 1}
            </Button>
          ))}
          {endPage < totalPages - 1 && (
            <>
              {endPage < totalPages - 2 && <span className="px-2 py-1">...</span>}
              <Button
                size="sm"
                variant="secondary"
                onClick={() => handlePageChange(totalPages - 1)}
              >
                {totalPages}
              </Button>
            </>
          )}
          <Button
            size="sm"
            variant="secondary"
            onClick={() => handlePageChange(currentPage + 1)}
            disabled={currentPage === totalPages - 1}
          >
            Next
          </Button>
        </div>
      </div>
    );
  };

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">Cards</h1>
          <p className="text-sm text-gray-600 mt-1">Manage credit and debit cards</p>
        </div>
        <Button onClick={() => router.push('/cards/new')}>
          Create New Card
        </Button>
      </div>

      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <h2 className="text-lg font-semibold text-gray-900 mb-4">Search Filters</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          <Input
            label="Account ID"
            placeholder="Enter account ID (11 digits)"
            value={accountIdFilter}
            onChange={(e) => setAccountIdFilter(e.target.value)}
          />
          <Input
            label="Card Number"
            placeholder="Enter card number (16 digits)"
            value={cardNumberFilter}
            onChange={(e) => setCardNumberFilter(e.target.value)}
          />
          <div className="flex items-end gap-2">
            <Button onClick={handleSearch} className="flex-1">
              Search
            </Button>
            <Button
              variant="secondary"
              onClick={handleClearFilters}
              className="flex-1"
            >
              Clear
            </Button>
          </div>
        </div>
        {(appliedAccountId || appliedCardNumber) && (
          <div className="mt-4 flex flex-wrap gap-2">
            <span className="text-sm text-gray-600">Active filters:</span>
            {appliedAccountId && (
              <span className="inline-flex items-center px-3 py-1 rounded-full text-sm bg-blue-100 text-blue-800">
                Account ID: {appliedAccountId}
                <button
                  onClick={() => {
                    setAccountIdFilter('');
                    setAppliedAccountId('');
                    setCurrentPage(0);
                  }}
                  className="ml-2 text-blue-600 hover:text-blue-800"
                >
                  ×
                </button>
              </span>
            )}
            {appliedCardNumber && (
              <span className="inline-flex items-center px-3 py-1 rounded-full text-sm bg-blue-100 text-blue-800">
                Card Number: {appliedCardNumber}
                <button
                  onClick={() => {
                    setCardNumberFilter('');
                    setAppliedCardNumber('');
                    setCurrentPage(0);
                  }}
                  className="ml-2 text-blue-600 hover:text-blue-800"
                >
                  ×
                </button>
              </span>
            )}
          </div>
        )}
      </div>

      {loading ? (
        <div className="flex items-center justify-center py-12">
          <div className="text-center">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto mb-4"></div>
            <p className="text-gray-600">Loading cards...</p>
          </div>
        </div>
      ) : error ? (
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <div className="flex items-center">
            <span className="text-red-600 text-xl mr-3">⚠️</span>
            <div>
              <h3 className="text-red-800 font-semibold">Error Loading Cards</h3>
              <p className="text-red-600">{error}</p>
            </div>
          </div>
          <div className="mt-4">
            <Button onClick={fetchCards}>Retry</Button>
          </div>
        </div>
      ) : cards.length === 0 ? (
        <div className="bg-white rounded-lg shadow p-12 text-center">
          <div className="text-gray-400 text-6xl mb-4">💳</div>
          <h3 className="text-lg font-semibold text-gray-900 mb-2">No Cards Found</h3>
          <p className="text-gray-600 mb-6">
            {appliedAccountId || appliedCardNumber
              ? 'No cards match your search criteria. Try adjusting your filters.'
              : 'Get started by creating your first card.'}
          </p>
          {(appliedAccountId || appliedCardNumber) ? (
            <Button variant="secondary" onClick={handleClearFilters}>
              Clear Filters
            </Button>
          ) : (
            <Button onClick={() => router.push('/cards/new')}>
              Create New Card
            </Button>
          )}
        </div>
      ) : (
        <>
          <div className="bg-white rounded-lg shadow overflow-hidden">
            <Table
              columns={[
                {
                  key: 'cardNumber',
                  label: 'Card Number',
                  render: (card: CardListItem) => (
                    <span className="font-mono text-sm">{maskCardNumber(card.cardNumber)}</span>
                  ),
                },
                {
                  key: 'accountId',
                  label: 'Account ID',
                  render: (card: CardListItem) => (
                    <span className="font-mono text-sm">{card.accountId}</span>
                  ),
                },
                {
                  key: 'embossedName',
                  label: 'Embossed Name',
                  render: (card: CardListItem) => (
                    <span className="font-medium">{card.embossedName}</span>
                  ),
                },
                {
                  key: 'expirationDate',
                  label: 'Expiration Date',
                  render: (card: CardListItem) => (
                    <span className={card.expired ? 'text-red-600 font-semibold' : ''}>
                      {formatDate(card.expirationDate)}
                      {card.expired && ' (Expired)'}
                    </span>
                  ),
                },
                {
                  key: 'activeStatus',
                  label: 'Status',
                  render: (card: CardListItem) => (
                    <div className="flex flex-col gap-1">
                      <span
                        className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                          card.activeStatus === 'Y'
                            ? 'bg-green-100 text-green-800'
                            : 'bg-red-100 text-red-800'
                        }`}
                      >
                        {card.activeStatus === 'Y' ? 'Active' : 'Inactive'}
                      </span>
                      {card.expired && (
                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                          Expired
                        </span>
                      )}
                    </div>
                  ),
                },
              ]}
              data={cards}
              onRowClick={(card) => router.push(`/cards/${card.cardNumber}`)}
              actions={(card) => (
                <div className="flex gap-2">
                  <Button
                    size="sm"
                    onClick={(e) => {
                      e.stopPropagation();
                      router.push(`/cards/${card.cardNumber}/edit`);
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
          {renderPagination()}
        </>
      )}
    </div>
  );
}
