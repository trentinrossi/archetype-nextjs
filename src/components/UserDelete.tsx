'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { User, APIResponse } from '@/types/user';
import { userService } from '@/services/userService';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';

interface UserDeleteProps {
  onUserDeleted?: (userId: string) => void;
  onExit?: () => void;
  className?: string;
}

export const UserDelete: React.FC<UserDeleteProps> = ({
  onUserDeleted,
  onExit,
  className = '',
}) => {
  const [lookupUserId, setLookupUserId] = useState<string>('');
  const [currentUser, setCurrentUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(false);
  const [lookupLoading, setLookupLoading] = useState(false);
  const [successMessage, setSuccessMessage] = useState<string>('');
  const [generalError, setGeneralError] = useState<string>('');
  const [isConfirmMode, setIsConfirmMode] = useState(false);
  const [showUserDetails, setShowUserDetails] = useState(false);

  const clearMessages = useCallback(() => {
    setSuccessMessage('');
    setGeneralError('');
  }, []);

  const handleLookupUserIdChange = useCallback((value: string) => {
    clearMessages();
    setLookupUserId(value.toUpperCase().slice(0, 8));
  }, [clearMessages]);

  const handleLookupUser = useCallback(async () => {
    if (!lookupUserId.trim()) {
      setGeneralError('Please enter a User ID to lookup');
      return;
    }

    if (lookupUserId.length !== 8) {
      setGeneralError('User ID must be exactly 8 characters');
      return;
    }

    clearMessages();
    setLookupLoading(true);
    setCurrentUser(null);
    setShowUserDetails(false);
    setIsConfirmMode(false);

    try {
      const response: APIResponse<User> = await userService.getUserById(lookupUserId);

      if (response.success && response.data) {
        const user = response.data;
        setCurrentUser(user);
        setShowUserDetails(true);
        setSuccessMessage(`User ${user.userId} found and loaded for deletion`);
      } else {
        // Handle API errors
        if (response.error?.statusCode === 404) {
          setGeneralError(`User ${lookupUserId} not found`);
        } else {
          setGeneralError(response.error?.message || 'Failed to lookup user. Please try again.');
        }
      }
    } catch (error) {
      setGeneralError('System error occurred during lookup. Please try again later.');
      console.error('User lookup error:', error);
    } finally {
      setLookupLoading(false);
    }
  }, [lookupUserId, clearMessages]);

  const handleDeleteConfirm = useCallback(() => {
    if (!currentUser) {
      setGeneralError('No user loaded for deletion');
      return;
    }

    clearMessages();
    setIsConfirmMode(true);
  }, [currentUser, clearMessages]);

  const handleDeleteExecute = useCallback(async () => {
    if (!currentUser) {
      setGeneralError('No user loaded for deletion');
      return;
    }

    clearMessages();
    setLoading(true);

    try {
      const response: APIResponse<void> = await userService.deleteUser(currentUser.userId);

      if (response.success) {
        const deletedUserId = currentUser.userId;
        setSuccessMessage(`User ${deletedUserId} has been successfully deleted!`);
        
        // Clear user data after successful deletion
        setCurrentUser(null);
        setShowUserDetails(false);
        setIsConfirmMode(false);
        setLookupUserId('');
        
        if (onUserDeleted) {
          onUserDeleted(deletedUserId);
        }
      } else {
        // Handle API errors
        if (response.error?.statusCode === 404) {
          setGeneralError('User not found for deletion');
        } else if (response.error?.statusCode === 403) {
          setGeneralError('You do not have permission to delete this user');
        } else if (response.error?.statusCode === 409) {
          setGeneralError('Cannot delete user - user may have associated data');
        } else {
          setGeneralError(response.error?.message || 'Failed to delete user. Please try again.');
        }
        setIsConfirmMode(false);
      }
    } catch (error) {
      setGeneralError('System error occurred during deletion. Please try again later.');
      setIsConfirmMode(false);
      console.error('User deletion error:', error);
    } finally {
      setLoading(false);
    }
  }, [currentUser, clearMessages, onUserDeleted]);

  const handleCancelDelete = useCallback(() => {
    clearMessages();
    setIsConfirmMode(false);
  }, [clearMessages]);

  const handleClear = useCallback(() => {
    setLookupUserId('');
    setCurrentUser(null);
    setShowUserDetails(false);
    setIsConfirmMode(false);
    clearMessages();
  }, [clearMessages]);

  const handleExit = useCallback(() => {
    if (onExit) {
      onExit();
    }
  }, [onExit]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      switch (event.key) {
        case 'Enter':
          event.preventDefault();
          if (!lookupLoading && !loading) {
            if (!showUserDetails) {
              handleLookupUser();
            } else if (isConfirmMode) {
              handleDeleteExecute();
            } else {
              handleDeleteConfirm();
            }
          }
          break;
        case 'F3':
          event.preventDefault();
          handleExit();
          break;
        case 'F4':
          event.preventDefault();
          handleClear();
          break;
        case 'F5':
          event.preventDefault();
          if (showUserDetails && !isConfirmMode) {
            handleDeleteConfirm();
          } else if (isConfirmMode) {
            handleDeleteExecute();
          }
          break;
        case 'F12':
        case 'Escape':
          event.preventDefault();
          if (isConfirmMode) {
            handleCancelDelete();
          } else {
            handleExit();
          }
          break;
      }
    };

    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [
    lookupLoading,
    loading,
    showUserDetails,
    isConfirmMode,
    handleLookupUser,
    handleDeleteConfirm,
    handleDeleteExecute,
    handleClear,
    handleExit,
    handleCancelDelete,
  ]);

  const formatUserType = (userType: 'A' | 'U'): string => {
    return userType === 'A' ? 'A - Admin' : 'U - User';
  };

  return (
    <div className={`bg-background border border-border rounded-lg p-6 max-w-2xl mx-auto ${className}`}>
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-foreground mb-2">Delete User - COUSR03C</h1>
        <p className="text-sm text-muted-foreground">
          {!showUserDetails 
            ? 'Enter User ID and press ENTER to lookup user for deletion'
            : isConfirmMode
            ? 'Press PF5 to confirm deletion or PF12 to cancel'
            : 'Review user details and press PF5 to confirm deletion'
          }
        </p>
      </div>

      {/* Success Message */}
      {successMessage && (
        <div className="mb-4 p-3 bg-green-50 border border-green-200 rounded-md">
          <p className="text-green-800 text-sm font-medium">{successMessage}</p>
        </div>
      )}

      {/* General Error Message */}
      {generalError && (
        <div className="mb-4 p-3 bg-destructive/10 border border-destructive/20 rounded-md">
          <p className="text-destructive text-sm">{generalError}</p>
        </div>
      )}

      {/* User Lookup Section */}
      {!showUserDetails && (
        <div className="mb-6 p-4 bg-muted/30 rounded-md">
          <div className="flex gap-2">
            <div className="flex-1">
              <Input
                label="User ID to Delete *"
                value={lookupUserId}
                onChange={(e) => handleLookupUserIdChange(e.target.value)}
                maxLength={8}
                placeholder="Enter 8-character User ID"
                className="font-mono"
                disabled={lookupLoading}
              />
            </div>
            <div className="flex items-end">
              <Button
                onClick={handleLookupUser}
                disabled={lookupLoading || !lookupUserId.trim()}
                variant="primary"
                className="mb-1"
              >
                {lookupLoading ? 'Looking up...' : 'Lookup (ENTER)'}
              </Button>
            </div>
          </div>
          <p className="text-xs text-muted-foreground mt-2">
            User ID: {lookupUserId.length}/8 characters
          </p>
        </div>
      )}

      {/* User Details Display */}
      {showUserDetails && currentUser && (
        <div className="mb-6">
          {/* Current User Info Header */}
          <div className="mb-4 p-3 bg-destructive/5 border border-destructive/20 rounded-md">
            <p className="text-sm font-medium text-destructive">
              User Selected for Deletion: <span className="font-mono">{currentUser.userId}</span>
            </p>
            {currentUser.createdAt && (
              <p className="text-xs text-muted-foreground">
                Created: {new Date(currentUser.createdAt).toLocaleString()}
              </p>
            )}
          </div>

          {/* User Details (Read-only) */}
          <div className="space-y-4 p-4 bg-muted/20 rounded-md">
            <h3 className="text-lg font-semibold text-foreground mb-3">User Details</h3>
            
            {/* User ID */}
            <div>
              <label className="block text-sm font-medium mb-2">User ID</label>
              <div className="flex h-10 w-full rounded-md border border-border bg-muted px-3 py-2 text-sm font-mono text-muted-foreground">
                {currentUser.userId}
              </div>
            </div>

            {/* First Name */}
            <div>
              <label className="block text-sm font-medium mb-2">First Name</label>
              <div className="flex h-10 w-full rounded-md border border-border bg-muted px-3 py-2 text-sm text-muted-foreground">
                {currentUser.firstName}
              </div>
            </div>

            {/* Last Name */}
            <div>
              <label className="block text-sm font-medium mb-2">Last Name</label>
              <div className="flex h-10 w-full rounded-md border border-border bg-muted px-3 py-2 text-sm text-muted-foreground">
                {currentUser.lastName}
              </div>
            </div>

            {/* User Type */}
            <div>
              <label className="block text-sm font-medium mb-2">User Type</label>
              <div className="flex h-10 w-full rounded-md border border-border bg-muted px-3 py-2 text-sm text-muted-foreground">
                {formatUserType(currentUser.userType)}
              </div>
            </div>
          </div>

          {/* Confirmation Mode */}
          {isConfirmMode && (
            <div className="mt-4 p-4 bg-destructive/10 border-2 border-destructive/30 rounded-md">
              <div className="flex items-center gap-2 mb-3">
                <div className="w-6 h-6 bg-destructive rounded-full flex items-center justify-center">
                  <span className="text-destructive-foreground text-sm font-bold">!</span>
                </div>
                <h3 className="text-lg font-bold text-destructive">Confirm Deletion</h3>
              </div>
              <p className="text-destructive text-sm mb-3">
                <strong>WARNING:</strong> You are about to permanently delete user <span className="font-mono font-bold">{currentUser.userId}</span>.
              </p>
              <p className="text-destructive text-sm mb-3">
                This action cannot be undone. All user data will be permanently removed from the system.
              </p>
              <p className="text-destructive text-sm font-medium">
                Press PF5 to confirm deletion or PF12 to cancel.
              </p>
            </div>
          )}
        </div>
      )}

      {/* Action Buttons */}
      <div className="flex justify-between items-center mt-6 pt-4 border-t border-border">
        {showUserDetails ? (
          <>
            <div className="flex gap-2">
              {!isConfirmMode ? (
                <Button
                  onClick={handleDeleteConfirm}
                  disabled={loading}
                  variant="destructive"
                >
                  Delete User (PF5)
                </Button>
              ) : (
                <>
                  <Button
                    onClick={handleDeleteExecute}
                    disabled={loading}
                    variant="destructive"
                  >
                    {loading ? 'Deleting...' : 'Confirm Delete (PF5)'}
                  </Button>
                  <Button
                    onClick={handleCancelDelete}
                    disabled={loading}
                    variant="outline"
                  >
                    Cancel (PF12)
                  </Button>
                </>
              )}
              <Button
                onClick={handleClear}
                disabled={loading}
                variant="outline"
              >
                Clear (PF4)
              </Button>
            </div>

            <Button
              onClick={handleExit}
              disabled={loading}
              variant="outline"
            >
              Exit (PF3)
            </Button>
          </>
        ) : (
          <div className="flex justify-between w-full">
            <Button
              onClick={handleClear}
              disabled={lookupLoading}
              variant="outline"
            >
              Clear (PF4)
            </Button>
            <Button
              onClick={handleExit}
              disabled={lookupLoading}
              variant="outline"
            >
              Exit (PF3)
            </Button>
          </div>
        )}
      </div>

      {/* Keyboard Shortcuts Help */}
      <div className="mt-4 pt-4 border-t border-border">
        <p className="text-xs text-muted-foreground">
          <strong>Keyboard Shortcuts:</strong> 
          {!showUserDetails 
            ? ' ENTER = Lookup User | PF3 = Exit | PF4 = Clear | ESC = Exit'
            : isConfirmMode
            ? ' PF5 = Confirm Delete | PF12 = Cancel | PF3 = Exit | ESC = Cancel'
            : ' PF5 = Delete | PF3 = Exit | PF4 = Clear | ESC = Exit'
          }
        </p>
      </div>

      {/* Safety Warning */}
      {showUserDetails && (
        <div className="mt-4 p-3 bg-orange-50 border border-orange-200 rounded-md">
          <p className="text-xs text-orange-800">
            <strong>Safety Notice:</strong> User deletion is permanent and cannot be undone. 
            Ensure you have selected the correct user before confirming deletion.
          </p>
        </div>
      )}
    </div>
  );
};

export default UserDelete;